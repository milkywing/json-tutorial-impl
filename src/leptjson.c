
#include "leptjson.h"

#include <assert.h> /* assert() */
#include <errno.h>  /* errno, ERANGE */
#include <math.h>   /* HUGE_VAL */
#include <stdio.h>  /* sprintf() */
#include <stdlib.h> /* NULL, malloc(), realloc(), free(), strtod() */
#include <string.h> /* memcpy() */

#define LEPT_STACK_INIT_SIZE 256

#define ISDIGIT(ch) ((ch) >= '0' && (ch) <= '9')
#define ISDIGIT1TO9(ch) ((ch) >= '1' && (ch) <= '9')

/* 向缓冲区中写入字符 */
#define PUTC(c, ch)                                    \
  do {                                                 \
    *(char*)lept_context_push(c, sizeof(char)) = (ch); \
  } while (0)
/** 向缓冲区中写入字符串 */
#define PUTS(c, s, len) memcpy(lept_context_push(c, len), s, len)

typedef struct lept_context {
  const char* json; /* 当前解析位置 */
  char* stack;      /* 堆栈缓冲区 */
  size_t size;      /* 缓冲区容量（字节） */
  size_t top;       /* 缓冲区大小（字节） */
} lept_context;

/**
 * 在缓冲区中开辟 size 字节的空间，并返回空间起始地址
 * @param c pointer to context
 * @param size number of bytes to push
 * @return 空间起始地址
 */
static void* lept_context_push(lept_context* c, size_t size) {
  assert(size > 0);
  // 需要扩容
  if (c->top + size >= c->size) {
    if (c->size == 0) c->size = LEPT_STACK_INIT_SIZE;
    while (c->top + size >= c->size) c->size += c->size >> 1;
    c->stack = (char*)realloc(c->stack, c->size);
  }
  void* ret = c->stack + c->top;
  c->top += size;
  return ret;
}

/**
 * 从缓冲区中取出 size 字节的数据，并返回数据的起始地址
 * @param c pointer to context
 * @param size number of bytes to pop
 * @return 数据起始地址
 */
static void* lept_context_pop(lept_context* c, size_t size) {
  assert(c->top >= size);
  c->top -= size;
  return c->stack + c->top;
}

static void lept_skip_whitespace(lept_context* c) {
  const char* p = c->json;
  while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;
  c->json = p;
}

/**
 * 解析字面量（true/false/null），并将解析结果写入 JSON value
 * @param c pointer to context
 * @param v pointer to JSON value
 * @param literal literal name（true/false/null）
 * @param type type of JSON value
 * @return 错误码
 */
static int lept_parse_literal(lept_context* c, lept_value* v, const char* literal, lept_type type) {
  size_t i;
  for (i = 0; literal[i]; i++) {
    if (c->json[i] != literal[i]) return LEPT_PARSE_INVALID_VALUE;
  }
  c->json += i;
  v->type = type;
  return LEPT_PARSE_OK;
}

/**
 * 解析数值，并将解析结果写入 JSON value
 * @param c pointer to context
 * @param v pointer to JSON value
 * @return 错误码
 */
static int lept_parse_number(lept_context* c, lept_value* v) {
  const char* p = c->json;
  // JSON 数值的格式比 strtod 接受的格式严格，因此这里自行进行数值格式的校验
  // 1.可选的负号，-?
  if (*p == '-') p++;
  // 2.必选的整数部分，0|[1-9]\d*
  if (*p == '0') {
    p++;
  } else {
    if (!ISDIGIT1TO9(*p)) return LEPT_PARSE_INVALID_VALUE;
    for (p++; ISDIGIT(*p); p++) {
    }
  }
  // 3.可选的小数部分，(\.\d+)?
  if (*p == '.') {
    p++;
    if (!ISDIGIT(*p)) return LEPT_PARSE_INVALID_VALUE;
    for (p++; ISDIGIT(*p); p++) {
    }
  }
  // 4.可选的指数部分，([eE][+-]?\d+)?
  if (*p == 'e' || *p == 'E') {
    p++;
    if (*p == '+' || *p == '-') p++;
    // PS：指数部分是可以允许前导 0 的
    if (!ISDIGIT(*p)) return LEPT_PARSE_INVALID_VALUE;
    for (p++; ISDIGIT(*p); p++) {
    }
  }
  // 处理溢出情况
  v->u.n = strtod(c->json, NULL);
  if (v->u.n == HUGE_VAL || v->u.n == -HUGE_VAL) return LEPT_PARSE_NUMBER_TOO_BIG;
  v->type = LEPT_NUMBER;
  c->json = p;
  return LEPT_PARSE_OK;
}

/**
 * 解析 4 位十六进制数，将解析结果写入 u 并返回下一个解析的位置，如果解析失败则返回空指针
 * @param p pointer to the string to be interpreted
 * @param u pointer to parse result
 * @return 下一个解析的位置
 */
static const char* lept_parse_hex4(const char* p, unsigned* u) {
  unsigned res = 0;
  for (size_t i = 0; i < 4; i++) {
    char ch = *p++;
    res <<= 4;
    if (ch >= '0' && ch <= '9')
      res |= ch - '0';
    else if (ch >= 'A' && ch <= 'F')
      res |= ch - ('A' - 10);
    else if (ch >= 'a' && ch <= 'f')
      res |= ch - ('a' - 10);
    else
      return NULL;
  }
  *u = res;
  return p;
}

///| 码点范围            | 码点位数  | 字节1     | 字节2     | 字节3     | 字节4     |
///|:------------------:|:--------:|:--------:|:--------:|:--------:|:--------:|
///| U+0000 ~ U+007F    | 7        | 0xxxxxxx |
///| U+0080 ~ U+07FF    | 11       | 110xxxxx | 10xxxxxx |
///| U+0800 ~ U+FFFF    | 16       | 1110xxxx | 10xxxxxx | 10xxxxxx |
///| U+10000 ~ U+10FFFF | 21       | 11110xxx | 10xxxxxx | 10xxxxxx | 10xxxxxx |
/**
 * 将码点以 utf8 编码写入缓冲区
 * @param c pointer to context
 * @param u unicode code point
 */
static void lept_encode_utf8(lept_context* c, unsigned u) {
  unsigned char u8[4] = {0};
  size_t u8_len;
  if (u <= 0x7F) {
    u8[0] = u;
    u8_len = 1;
  } else if (u <= 0x7FF) {
    // utf8 码元分别为：码点的 6+ 位、码点的低 6 位
    u8[0] = 0xC0 | ((u >> 6) & 0xFF);
    u8[1] = 0x80 | (u & 0x3F);
    u8_len = 2;
  } else if (u <= 0xFFFF) {
    // utf8 码元分别为：码点的 12+ 位、码点的 7-12 位、码点的低 6 位
    u8[0] = 0xE0 | ((u >> 12) & 0xFF);
    u8[1] = 0x80 | ((u >> 6) & 0x3F);
    u8[2] = 0x80 | (u & 0x3F);
    u8_len = 3;
  } else {
    // utf8 码元分别为：码点的 18+ 位、码点的 13-18 位、码点的 7-12 位、码点的低 6 位
    assert(u <= 0x10FFFF);
    u8[0] = 0xF0 | ((u >> 18) & 0xFF);
    u8[1] = 0x80 | ((u >> 12) & 0x3F);
    u8[2] = 0x80 | ((u >> 6) & 0x3F);
    u8[3] = 0x80 | (u & 0x3F);
    u8_len = 4;
  }
  PUTS(c, u8, u8_len);
}

#define STRING_ERROR(ret) \
  do {                    \
    c->top = head;        \
    return ret;           \
  } while (0)

/**
 * 解析字符串，并通过 str 和 len 返回解析结果，其中 str 指向字符串在缓冲区中的位置（需及时使用）
 * @param c pointer to context
 * @param str pointer to parse result
 * @param len pointer to parse result length
 * @return 错误码
 */
static int lept_parse_string_raw(lept_context* c, char** str, size_t* len) {
  // 记录解析开始时缓冲区的大小，供解析结果长度的计算/解析失败时恢复
  size_t head = c->top;
  assert(*c->json == '"');
  c->json++;
  const char* p = c->json;
  // 代理对
  unsigned u, u2;
  while (1) {
    char ch = *p++;
    switch (ch) {
      case '"':
        *len = c->top - head;
        *str = lept_context_pop(c, *len);
        c->json = p;
        return LEPT_PARSE_OK;
      case '\\':
        switch (*p++) {
          case '"':
            PUTC(c, '"');
            break;
          case '\\':
            PUTC(c, '\\');
            break;
          case '/':
            PUTC(c, '/');
            break;
          case 'b':
            PUTC(c, '\b');
            break;
          case 'f':
            PUTC(c, '\f');
            break;
          case 'n':
            PUTC(c, '\n');
            break;
          case 'r':
            PUTC(c, '\r');
            break;
          case 't':
            PUTC(c, '\t');
            break;
          case 'u':
            // clang-format off
            /**
             * JSON 代理对详解：https://huangtengxiao.gitee.io/post/Unicode%E4%BB%A3%E7%90%86%E5%AF%B9.html
             * JSON 使用 \u 转义来表示 unicode 字符，并且这些 unicode 字符采用 utf-16 编码。
             * utf-16 属于变长编码，其码元大小为 16 位，unicode 字符需要使用需要 1 个或 2 个码元表示：
             * 1.当 unicode 字符的码点落在 BMP（基本多文种平面，范围 0000-FFFF）时，此时单个码元即可表示该码点，无需任何转换。
             * 2.当 unicode 字符的码点超出 BMP 范围（超出 16 位），就需要使用两个码元（也叫代理对）进行表示，
             * 其中高位码元的范围落在 D800-DBFF，低位码元的范围落在 DC00-DFFF（D800-DFFF 是 BMP 中预留给代理对的区域，这样在解析的过程中发现位于代理区的码元时，就知道接下来需要后续两个码元表示一个 BMP 范围外的字符），
             * 高位码元作为码点的高 10 位，低位码元作为码点的低 10 位，可表示高达 20 位的空间，又因代理对只表示 BMP 范围外的码元，实际上需要将这 20 位编码空间映射到 010000-10FFFF 对应的码点，
             * 映射的公式为：codepoint = 0x10000 + (H − 0xD800) × 0x400 + (L − 0xDC00)
             */
            // clang-format on
            if (!(p = lept_parse_hex4(p, &u))) STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_HEX);
            if (u >= 0xD800 && u <= 0xDFFF) {
              // 处理代理对，第一个必须为高代理项，后接一个低代理项
              if (u >= 0xDC00) STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_SURROGATE);
              if (*p++ == '\\' && *p++ == 'u') {
                if (!(p = lept_parse_hex4(p, &u2))) STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_HEX);
                if (u2 < 0xDC00 || u2 > 0xDFFF) STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_SURROGATE);
                u = 0x10000 + ((u - 0xD800) << 10) + (u2 - 0xDC00);
              } else {
                STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_SURROGATE);
              }
            }
            lept_encode_utf8(c, u);
            break;
          default:
            STRING_ERROR(LEPT_PARSE_INVALID_STRING_ESCAPE);
        }
        break;
      case '\0':
        STRING_ERROR(LEPT_PARSE_MISS_QUOTATION_MARK);
      default:
        if ((unsigned char)ch < 0x20) STRING_ERROR(LEPT_PARSE_INVALID_STRING_CHAR);
        PUTC(c, ch);
    }
  }
}

/**
 * 解析字符串，并将解析结果写入 JSON value
 * @param c pointer to context
 * @param v pointer to JSON value
 * @return 错误码
 */
static int lept_parse_string(lept_context* c, lept_value* v) {
  char* str;
  size_t len;
  int ret = lept_parse_string_raw(c, &str, &len);
  if (ret == LEPT_PARSE_OK) lept_set_string(v, str, len);
  return ret;
}

static int lept_parse_value(lept_context* c, lept_value* v);

/**
 * 解析数组，并将解析结果写入 JSON value
 * @param c pointer to context
 * @param v pointer to JSON value
 * @return 错误码
 */
static int lept_parse_array(lept_context* c, lept_value* v) {
  // Regex: \[\s*(value(\s*,\s*value)*)?\s*\]
  assert(*c->json == '[');
  c->json++;
  lept_skip_whitespace(c);
  // 空数组
  if (*c->json == ']') {
    c->json++;
    lept_set_array(v, 0);
    return LEPT_PARSE_OK;
  }
  // 非空数组，逐个解析元素
  int ret;
  size_t size = 0;
  while (1) {
    lept_value e;
    lept_init(&e);
    // 元素解析成功，将元素移动至缓冲区（准确来说在缓冲区中 placement new 元素）
    if ((ret = lept_parse_value(c, &e)) != LEPT_PARSE_OK) break;
    memcpy(lept_context_push(c, sizeof(lept_value)), &e, sizeof(lept_value));
    size++;
    // 一个元素后面除了空白，必须是逗号（存在后继元素）或者右中括号（数组结束）
    lept_skip_whitespace(c);
    if (*c->json == ',') {
      c->json++;
      lept_skip_whitespace(c);
    } else if (*c->json == ']') {
      c->json++;
      lept_set_array(v, size);
      memcpy(v->u.a.e, lept_context_pop(c, size * sizeof(lept_value)), size * sizeof(lept_value));
      v->u.a.size = size;
      return LEPT_PARSE_OK;
    } else {
      ret = LEPT_PARSE_MISS_COMMA_OR_SQUARE_BRACKET;
      break;
    }
  }
  // 解析失败，释放已经解析好的元素
  while (size--) {
    lept_free((lept_value*)lept_context_pop(c, sizeof(lept_value)));
  }
  return ret;
}

static void lept_free_member(lept_member* m) {
  free(m->k);
  lept_free(&m->v);
}

static void lept_swap_member(lept_member* lhs, lept_member* rhs) {
  assert(lhs != NULL && rhs != NULL);
  if (lhs != rhs) {
    lept_member temp;
    memcpy(&temp, lhs, sizeof(lept_member));
    memcpy(lhs, rhs, sizeof(lept_member));
    memcpy(rhs, &temp, sizeof(lept_member));
  }
}

/**
 * 解析对象，并将解析结果写入 JSON value
 * @param c pointer to context
 * @param v pointer to JSON value
 * @return 错误码
 */
static int lept_parse_object(lept_context* c, lept_value* v) {
  // Regex: \[\s*(kv(\s*,\s*kv)*)?\s*\]
  // kv Regex: string\s*:\s*value
  assert(*c->json == '{');
  c->json++;
  lept_skip_whitespace(c);
  // 空对象
  if (*c->json == '}') {
    c->json++;
    lept_set_object(v, 0);
    return LEPT_PARSE_OK;
  }
  // 非空对象，逐个解析键值对
  int ret;
  size_t size = 0;
  /**
   * 和数组解析不同，这里把键值对提到了循环外面，在循环里面重复使用，
   * 这样在键已分配内存但后续解析出错情况下，可以避免在多处 break 的地方释放键占有的内存，转而统一在最后释放掉
   */
  lept_member m;
  m.k = NULL;
  lept_init(&m.v);
  while (1) {
    // 解析键
    if (*c->json != '"') {
      ret = LEPT_PARSE_MISS_KEY;
      break;
    }
    char* key_str;
    if ((ret = lept_parse_string_raw(c, &key_str, &m.klen)) != LEPT_PARSE_OK) break;
    memcpy(m.k = (char*)malloc(m.klen + 1), key_str, m.klen);
    m.k[m.klen] = '\0';
    // 冒号
    lept_skip_whitespace(c);
    if (*c->json != ':') {
      ret = LEPT_PARSE_MISS_COLON;
      break;
    }
    c->json++;
    lept_skip_whitespace(c);
    // 解析值成功后，将键值对移动至缓冲区（准确来说在缓冲区中 placement new 键值对）
    if ((ret = lept_parse_value(c, &m.v)) != LEPT_PARSE_OK) break;
    memcpy(lept_context_push(c, sizeof(lept_member)), &m, sizeof(lept_member));
    size++;
    m.k = NULL;
    lept_init(&m.v);
    // 一个键值对后面除了空白，必须是逗号（存在后继键值对）或者右大括号（对象结束）
    lept_skip_whitespace(c);
    if (*c->json == ',') {
      c->json++;
      lept_skip_whitespace(c);
    } else if (*c->json == '}') {
      c->json++;
      lept_set_object(v, size);
      memcpy(v->u.o.m, lept_context_pop(c, size * sizeof(lept_member)), size * sizeof(lept_member));
      v->u.o.size = size;
      return LEPT_PARSE_OK;
    } else {
      ret = LEPT_PARSE_MISS_COMMA_OR_CURLY_BRACKET;
      break;
    }
  }
  // 如果某个键值对的键已经分配了内存，但是该键值对解析失败了，需要释放掉
  free(m.k);
  while (size--) {
    lept_free_member(lept_context_pop(c, sizeof(lept_member)));
  }
  return ret;
}

/**
 * 解析 JSON，并将解析结果写入 JSON value
 * @param c pointer to context
 * @param v pointer to JSON value
 * @return 错误码
 */
static int lept_parse_value(lept_context* c, lept_value* v) {
  if (*c->json == '\0') return LEPT_PARSE_EXPECT_VALUE;
  switch (*c->json) {
    case 't':
      return lept_parse_literal(c, v, "true", LEPT_TRUE);
    case 'f':
      return lept_parse_literal(c, v, "false", LEPT_FALSE);
    case 'n':
      return lept_parse_literal(c, v, "null", LEPT_NULL);
    case '"':
      return lept_parse_string(c, v);
    case '[':
      return lept_parse_array(c, v);
    case '{':
      return lept_parse_object(c, v);
    default:
      return lept_parse_number(c, v);
  }
}

/**
 * 序列化字符串并写入缓冲区
 * @param c pointer to context
 * @param s string to be stringified
 * @param len length of s
 */
static void lept_stringify_string(lept_context* c, const char* s, size_t len) {
  static const char hex_digits[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
  /**
   * 分配足够大的缓冲区，在缓冲区中进行序列化，
   * 最糟的情况是所有字符都表示为\u00xx，加上头尾双引号，最多需要 6 * len + 2 字节
   */
  char *p, *head;
  size_t buffer_size = len * 6 + 2;
  p = head = lept_context_push(c, buffer_size);
  *p++ = '"';
  // 这里使用无符号类型承载字符是因为下面字符要和有符号数进行运算，要避免错误的符号扩展
  unsigned char ch;
  for (size_t i = 0; i < len; i++) {
    switch (ch = (unsigned char)s[i]) {
      case '"':
        *p++ = '\\';
        *p++ = '"';
        break;
      case '\\':
        *p++ = '\\';
        *p++ = '\\';
        break;
      case '\b':
        *p++ = '\\';
        *p++ = 'b';
        break;
      case '\f':
        *p++ = '\\';
        *p++ = 'f';
        break;
      case '\n':
        *p++ = '\\';
        *p++ = 'n';
        break;
      case '\r':
        *p++ = '\\';
        *p++ = 'r';
        break;
      case '\t':
        *p++ = '\\';
        *p++ = 't';
        break;
      default:
        // 码点小于 0x20 的字符需要表示为 \u00XX
        if (ch < 0x20) {
          *p++ = '\\';
          *p++ = 'u';
          *p++ = '0';
          *p++ = '0';
          *p++ = hex_digits[ch >> 4];
          *p++ = hex_digits[ch & 0xF];
        } else {
          *p++ = s[i];
        }
    }
  }
  *p++ = '"';
  // 剔除没用上的缓冲区大小
  c->top -= buffer_size - (p - head);
}

/**
 * 序列化 JSON value，并写入缓冲区
 * @param c pointer to context
 * @param v JSON value to be stringified
 */
static void lept_stringify_value(lept_context* c, const lept_value* v) {
  switch (v->type) {
    case LEPT_NULL:
      PUTS(c, "null", 4);
      break;
    case LEPT_TRUE:
      PUTS(c, "true", 4);
      break;
    case LEPT_FALSE:
      PUTS(c, "false", 5);
      break;
    case LEPT_NUMBER:
      // sprintf 会根据指数大小决定是否使用科学计数法表示，指定最大精度即可，这里 32 字节的字符串足够容纳（含结束标记）
      c->top -= 32 - sprintf(lept_context_push(c, 32), "%.17g", v->u.n);
      break;
    case LEPT_STRING:
      lept_stringify_string(c, v->u.s.s, v->u.s.len);
      break;
    case LEPT_ARRAY:
      PUTC(c, '[');
      for (size_t i = 0; i < v->u.a.size; i++) {
        if (i > 0) PUTC(c, ',');
        lept_stringify_value(c, v->u.a.e + i);
      }
      PUTC(c, ']');
      break;
    case LEPT_OBJECT:
      PUTC(c, '{');
      for (size_t i = 0; i < v->u.o.size; i++) {
        if (i > 0) PUTC(c, ',');
        lept_member* m = v->u.o.m + i;
        lept_stringify_string(c, m->k, m->klen);
        PUTC(c, ':');
        lept_stringify_value(c, &m->v);
      }
      PUTC(c, '}');
      break;
    default:
      assert(0 && "invalid type");
  }
}

// ===================== Exposed functions =====================

/**
 * 解析 JSON，并将解析结果写入 JSON value
 * @param v pointer to JSON value
 * @param json JSON to be parsed
 * @return 错误码
 */
int lept_parse(lept_value* v, const char* json) {
  assert(v != NULL);
  lept_init(v);
  // 初始化上下文
  lept_context c;
  c.json = json;
  c.stack = NULL;
  c.size = c.top = 0;
  lept_skip_whitespace(&c);
  int ret = lept_parse_value(&c, v);
  // 如果解析成功，需要检查后继除了空白是否还存在非空字符
  if (ret == LEPT_PARSE_OK) {
    lept_skip_whitespace(&c);
    if (*c.json != '\0') {
      v->type = LEPT_NULL;
      ret = LEPT_PARSE_ROOT_NOT_SINGULAR;
    }
  }
  // 正常来说无论解析成功还是失败，缓冲区的大小都是 0
  assert(c.top == 0);
  free(c.stack);
  return ret;
}

/**
 * 序列化 JSON value，返回序列化结果及其长度
 * @param v JSON value to be stringified
 * @param length pointer to length of stringify result, set NULL if you don't need
 * @return stringify result
 */
char* lept_stringify(const lept_value* v, size_t* length) {
  assert(v != NULL);
  lept_context c;
  c.stack = malloc(c.size = LEPT_STACK_INIT_SIZE);
  c.top = 0;
  lept_stringify_value(&c, v);
  if (length) *length = c.top;
  PUTC(&c, '\0');
  return c.stack;
}

/**
 * 释放 JSON value 占有的内存，并设为 null
 * @param v pointer to JSON value
 */
void lept_free(lept_value* v) {
  assert(v != NULL);
  size_t i;
  switch (v->type) {
    case LEPT_STRING:
      free(v->u.s.s);
      break;
    case LEPT_ARRAY:
      for (i = 0; i < v->u.a.size; i++) lept_free(v->u.a.e + i);
      free(v->u.a.e);
      break;
    case LEPT_OBJECT:
      for (i = 0; i < v->u.o.size; i++) lept_free_member(v->u.o.m + i);
      free(v->u.o.m);
      break;
    default:
      break;
  }
  v->type = LEPT_NULL;
}

/**
 * 深拷贝 JSON value
 * @param dst pointer to destination
 * @param src pointer to source
 */
void lept_copy(lept_value* dst, const lept_value* src) {
  assert(dst != NULL && src != NULL && dst != src);
  switch (src->type) {
    case LEPT_STRING:
      lept_set_string(dst, src->u.s.s, src->u.s.len);
      break;
    case LEPT_ARRAY:
      lept_set_array(dst, src->u.a.size);
      dst->u.a.size = src->u.a.size;
      for (size_t i = 0; i < src->u.a.size; i++) {
        lept_init(dst->u.a.e + i);
        lept_copy(dst->u.a.e + i, src->u.a.e + i);
      }
      break;
    case LEPT_OBJECT:
      lept_set_object(dst, src->u.o.size);
      dst->u.o.size = src->u.o.size;
      for (size_t i = 0; i < src->u.o.size; i++) {
        lept_member *dst_m = dst->u.o.m + i, *src_m = src->u.o.m + i;
        size_t klen = src_m->klen;
        memcpy(dst_m->k = malloc(klen + 1), src_m->k, klen);
        dst_m->klen = klen;
        dst_m->k[klen] = '\0';
        lept_init(&dst_m->v);
        lept_copy(&dst_m->v, &src_m->v);
      }
      break;
    default:
      lept_free(dst);
      memcpy(dst, src, sizeof(lept_value));
  }
}

/**
 * 移动 JSON value
 * @param dst pointer to destination
 * @param src pointer to source
 */
void lept_move(lept_value* dst, lept_value* src) {
  assert(dst != NULL && src != NULL && src != dst);
  lept_free(dst);
  memcpy(dst, src, sizeof(lept_value));
  lept_init(src);
}

/** 交换 JSON value */
void lept_swap(lept_value* lhs, lept_value* rhs) {
  assert(lhs != NULL && rhs != NULL);
  if (lhs != rhs) {
    lept_value temp;
    memcpy(&temp, lhs, sizeof(lept_value));
    memcpy(lhs, rhs, sizeof(lept_value));
    memcpy(rhs, &temp, sizeof(lept_value));
  }
}

/** 深度比较 JSON value */
int lept_is_equal(const lept_value* lhs, const lept_value* rhs) {
  assert(lhs != NULL && rhs != NULL);
  if (lhs->type != rhs->type) return 0;
  switch (lhs->type) {
    case LEPT_NUMBER:
      return lhs->u.n == rhs->u.n;
    case LEPT_STRING:
      return lhs->u.s.len == rhs->u.s.len && !memcmp(lhs->u.s.s, rhs->u.s.s, lhs->u.s.len);
    case LEPT_ARRAY:
      if (lhs->u.a.size != rhs->u.a.size) return 0;
      for (size_t i = 0; i < lhs->u.a.size; i++) {
        if (!lept_is_equal(lhs->u.a.e + i, rhs->u.a.e + i)) return 0;
      }
      return 1;
    case LEPT_OBJECT:
      if (lhs->u.o.size != rhs->u.o.size) return 0;
      for (size_t i = 0; i < lhs->u.o.size; i++) {
        const lept_member* m = lhs->u.o.m + i;
        const lept_value* find = lept_find_object_value(rhs, m->k, m->klen);
        if (!find || !lept_is_equal(&m->v, find)) return 0;
      }
      return 1;
    default:
      return 1;
  }
}

lept_type lept_get_type(const lept_value* v) {
  assert(v != NULL);
  return v->type;
}

int lept_get_boolean(const lept_value* v) {
  assert(v != NULL && (v->type == LEPT_TRUE || v->type == LEPT_FALSE));
  return v->type == LEPT_TRUE;
}

void lept_set_boolean(lept_value* v, int b) {
  lept_free(v);
  v->type = b ? LEPT_TRUE : LEPT_FALSE;
}

double lept_get_number(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_NUMBER);
  return v->u.n;
}

void lept_set_number(lept_value* v, double n) {
  lept_free(v);
  v->u.n = n;
  v->type = LEPT_NUMBER;
}

const char* lept_get_string(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_STRING);
  return v->u.s.s;
}

size_t lept_get_string_length(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_STRING);
  return v->u.s.len;
}

/**
 * 将一个 JSON value 写为字符串
 * @param v pointer to JSON value
 * @param s source string
 * @param len source string length
 */
void lept_set_string(lept_value* v, const char* s, size_t len) {
  assert(v != NULL && (s != NULL || len == 0));
  lept_free(v);
  memcpy(v->u.s.s = (char*)malloc(len + 1), s, len);
  v->u.s.s[len] = '\0';
  v->u.s.len = len;
  v->type = LEPT_STRING;
}

// ===================== Array operations（No bounds checking） =====================

size_t lept_get_array_size(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  return v->u.a.size;
}

size_t lept_get_array_capacity(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  return v->u.a.capacity;
}

lept_value* lept_get_array_element(lept_value* v, size_t index) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  assert(index < v->u.a.size);
  return &v->u.a.e[index];
}

/**
 * 将一个 JSON value 写为指定容量的数组（未初始化）
 * @param v pointer to JSON value
 * @param capacity init capacity of array
 */
void lept_set_array(lept_value* v, size_t capacity) {
  assert(v != NULL);
  lept_free(v);
  v->u.a.size = 0;
  v->u.a.capacity = capacity;
  v->u.a.e = capacity > 0 ? (lept_value*)malloc(capacity * sizeof(lept_value)) : NULL;
  v->type = LEPT_ARRAY;
}

/**
 * 为数组预留空间，如果要求的容量大于数组容量会进行内存重新分配，否则无操作
 * @param v pointer to JSON value
 * @param capacity new capacity of the array, in number of elements
 */
void lept_reserve_array(lept_value* v, size_t capacity) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  if (v->u.a.capacity < capacity) {
    v->u.a.capacity = capacity;
    v->u.a.e = (lept_value*)realloc(v->u.a.e, capacity * sizeof(lept_value));
  }
}

/**
 * 将数组缩容至其大小
 * @param v pointer to JSON value
 */
void lept_shrink_array(lept_value* v) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  if (v->u.a.capacity > v->u.a.size) {
    v->u.a.capacity = v->u.a.size;
    v->u.a.e = (lept_value*)realloc(v->u.a.e, v->u.a.capacity * sizeof(lept_value));
  }
}

void lept_clear_array(lept_value* v) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  lept_erase_array_element(v, 0, v->u.a.size);
}

/**
 * 在数组尾部插入元素，返回指向插入位置的指针
 * @param v pointer to JSON value
 * @return 指向插入位置的指针
 */
lept_value* lept_pushback_array_element(lept_value* v) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  if (v->u.a.size == v->u.a.capacity) lept_reserve_array(v, v->u.a.capacity == 0 ? 1 : v->u.a.capacity * 2);
  lept_init(v->u.a.e + v->u.a.size);
  return v->u.a.e + v->u.a.size++;
}

/**
 * 删除数组尾部元素
 * @param v pointer to JSON value
 */
void lept_popback_array_element(lept_value* v) {
  assert(v != NULL && v->type == LEPT_ARRAY && v->u.a.size > 0);
  lept_free(v->u.a.e + --v->u.a.size);
}

/**
 * 在 index 位置插入元素，返回指向插入位置的指针
 * @param v pointer to JSON value
 * @param index index to insert
 * @return 指向插入位置的指针
 */
lept_value* lept_insert_array_element(lept_value* v, size_t index) {
  assert(v != NULL && v->type == LEPT_ARRAY && index <= v->u.a.size);
  size_t pre_size = v->u.a.size;
  // 需要扩容
  if (pre_size + 1 > v->u.a.capacity) {
    lept_reserve_array(v, v->u.a.capacity == 0 ? 1 : v->u.a.capacity * 2);
  }
  // 处理后续元素的移动
  for (size_t i = pre_size; i > index; i--) {
    lept_swap(v->u.a.e + i - 1, v->u.a.e + i);
  }
  lept_init(v->u.a.e + index);
  v->u.a.size++;
  return v->u.a.e + index;
}

/**
 * 从 index 位置开始，删除数组的 count 个 元素
 * @param v pointer to JSON value
 * @param index start index
 * @param count count of elements to erase
 */
void lept_erase_array_element(lept_value* v, size_t index, size_t count) {
  assert(v != NULL && v->type == LEPT_ARRAY && index + count <= v->u.a.size);
  if (!count) return;
  size_t i;
  // 处理后续元素的移动
  for (i = index; i < v->u.a.size - count; i++) {
    lept_move(v->u.a.e + i, v->u.a.e + i + count);
  }
  // 处理剩余元素的删除
  for (; i < index + count; i++) {
    lept_free(v->u.a.e + i);
  }
  v->u.a.size -= count;
}

// ===================== Object operations（No bounds checking） =====================

size_t lept_get_object_size(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  return v->u.o.size;
}

size_t lept_get_object_capacity(const lept_value* v) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  return v->u.o.capacity;
}

const char* lept_get_object_key(const lept_value* v, size_t index) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  assert(index < v->u.o.size);
  return v->u.o.m[index].k;
}

size_t lept_get_object_key_length(const lept_value* v, size_t index) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  assert(index < v->u.o.size);
  return v->u.o.m[index].klen;
}

lept_value* lept_get_object_value(lept_value* v, size_t index) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  assert(index < v->u.o.size);
  return &v->u.o.m[index].v;
}

/**
 * 根据键查找对象中的值，返回其在对象中的下标，如果找不到返回 LEPT_KEY_NOT_EXIST
 * @param v pointer to JSON value
 * @param key key to find
 * @param klen length of key
 * @return 目标值在对象中的下标，如果找不到返回 LEPT_KEY_NOT_EXIST
 */
size_t lept_find_object_index(const lept_value* v, const char* key, size_t klen) {
  size_t i;
  assert(v != NULL && v->type == LEPT_OBJECT && key != NULL);
  for (i = 0; i < v->u.o.size; i++) {
    if (v->u.o.m[i].klen == klen && !memcmp(v->u.o.m[i].k, key, klen)) return i;
  }
  return LEPT_KEY_NOT_EXIST;
}

/**
 * 根据键查找对象中的值，如果找不到返回 NULL
 * @param v pointer to JSON value
 * @param key key to find
 * @param klen length of key
 * @return 指向目标值的指针，如果找不到返回 NULL
 */
lept_value* lept_find_object_value(const lept_value* v, const char* key, size_t klen) {
  size_t index = lept_find_object_index(v, key, klen);
  return index != LEPT_KEY_NOT_EXIST ? &v->u.o.m[index].v : NULL;
}

/**
 * 将一个 JSON value 写为指定容量的对象（未初始化）
 * @param v pointer to JSON value
 * @param capacity init capacity of object
 */
void lept_set_object(lept_value* v, size_t capacity) {
  assert(v != NULL);
  lept_free(v);
  v->u.o.size = 0;
  v->u.o.capacity = capacity;
  v->u.o.m = capacity > 0 ? (lept_member*)malloc(capacity * sizeof(lept_member)) : NULL;
  v->type = LEPT_OBJECT;
}

/**
 * 为对象预留空间，如果要求的容量大于对象容量会进行内存重新分配，否则无操作
 * @param v pointer to JSON value
 * @param capacity new capacity of the array, in number of elements
 */
void lept_reserve_object(lept_value* v, size_t capacity) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  if (v->u.o.capacity < capacity) {
    v->u.o.capacity = capacity;
    v->u.o.m = (lept_member*)realloc(v->u.o.m, capacity * sizeof(lept_member));
  }
}

/**
 * 将对象缩容至其大小
 * @param v pointer to JSON value
 */
void lept_shrink_object(lept_value* v) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  if (v->u.o.size < v->u.o.capacity) {
    v->u.o.capacity = v->u.o.size;
    v->u.o.m = (lept_member*)realloc(v->u.o.m, v->u.o.capacity * sizeof(lept_member));
  }
}

void lept_clear_object(lept_value* v) {
  assert(v != NULL && v->type == LEPT_OBJECT);
  while (v->u.o.size) {
    lept_free_member(v->u.o.m + v->u.o.size - 1);
    v->u.o.size--;
  }
}

/**
 * 插入键值对，返回指向插入值的指针，如果键值对已经存在则进行覆盖
 * @param v pointer to JSON value
 * @param key key to set
 * @param klen length of key
 * @return 指向插入值的指针
 */
lept_value* lept_set_object_value(lept_value* v, const char* key, size_t klen) {
  assert(v != NULL && v->type == LEPT_OBJECT && key != NULL);
  size_t target_index;
  target_index = lept_find_object_index(v, key, klen);
  // 区分新增和更新场景
  lept_member* ret_m;
  if (target_index == LEPT_KEY_NOT_EXIST) {
    if (v->u.o.size + 1 > v->u.o.capacity) {
      lept_reserve_object(v, v->u.o.capacity == 0 ? 1 : v->u.o.capacity * 2);
    }
    ret_m = v->u.o.m + v->u.o.size;
    v->u.o.size++;
  } else {
    ret_m = v->u.o.m + target_index;
    lept_free_member(ret_m);
  }

  // 初始化写入的键值对
  memcpy(ret_m->k = malloc(klen + 1), key, klen);
  ret_m->k[klen] = '\0';
  ret_m->klen = klen;
  lept_init(&ret_m->v);
  return &ret_m->v;
}

/**
 * 删 index 位置的除键值对
 * @param v pointer to JSON value
 * @param index
 */
void lept_remove_object_value(lept_value* v, size_t index) {
  assert(v != NULL && v->type == LEPT_OBJECT && index < v->u.o.size);
  // 删除的位置是最后一个，直接释放掉
  if (index + 1 == v->u.o.size) {
    lept_free_member(v->u.o.m + index);
    v->u.o.size--;
    return;
  }
  // 删除的位置不是最后一个，处理后续键值对的移动
  size_t i;
  for (i = index; i < v->u.o.size - 1; i++) {
    lept_swap_member(v->u.o.m + i, v->u.o.m + i + 1);
  }
  lept_free_member(v->u.o.m + i);
  v->u.o.size--;
}
