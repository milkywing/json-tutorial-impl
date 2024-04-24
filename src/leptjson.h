#ifndef LEPTJSON_H__
#define LEPTJSON_H__

#include <stddef.h>

typedef enum {
  LEPT_NULL,
  LEPT_FALSE,
  LEPT_TRUE,
  LEPT_NUMBER,
  LEPT_STRING,
  LEPT_ARRAY,
  LEPT_OBJECT
} lept_type;

#define LEPT_KEY_NOT_EXIST ((size_t)-1)

typedef struct lept_value lept_value;
typedef struct lept_member lept_member;

struct lept_value {
  union {
    /* 对象-键值对集合（动态扩容） */
    struct {
      lept_member* m;
      size_t size, capacity;
    } o;
    /* 数组-元素集合（动态扩容） */
    struct {
      lept_value* e;
      size_t size, capacity;
    } a;
    /* 字符串 */
    struct {
      char* s;
      size_t len;
    } s;
    /* 数值 */
    double n;
  } u;
  lept_type type; /* 类型 */
};

struct lept_member {
  char* k;      /* 键 */
  size_t klen;  /* 键长 */
  lept_value v; /* 值 */
};

enum {
  LEPT_PARSE_OK,                           /* OK */
  LEPT_PARSE_EXPECT_VALUE,                 /* 解析目标为空 */
  LEPT_PARSE_INVALID_VALUE,                /* 非法值 */
  LEPT_PARSE_ROOT_NOT_SINGULAR,            /* JSON 值之后，除了空白还有其他字符 */
  LEPT_PARSE_NUMBER_TOO_BIG,               /* 数值-溢出 */
  LEPT_PARSE_MISS_QUOTATION_MARK,          /* 字符串-引号缺失 */
  LEPT_PARSE_INVALID_STRING_ESCAPE,        /* 字符串-非法转义 */
  LEPT_PARSE_INVALID_STRING_CHAR,          /* 字符串-非法字符 */
  LEPT_PARSE_INVALID_UNICODE_HEX,          /* 字符串-非法代理项 */
  LEPT_PARSE_INVALID_UNICODE_SURROGATE,    /* 字符串-非法代理对 */
  LEPT_PARSE_MISS_COMMA_OR_SQUARE_BRACKET, /* 数组-缺失逗号或结束中括号 */
  LEPT_PARSE_MISS_KEY,                     /* 对象-缺失键 */
  LEPT_PARSE_MISS_COLON,                   /* 对象-缺冒号 */
  LEPT_PARSE_MISS_COMMA_OR_CURLY_BRACKET   /* 对象-缺失逗号或结束大括号 */
};

#define lept_init(v)       \
  do {                     \
    (v)->type = LEPT_NULL; \
  } while (0)

int lept_parse(lept_value* v, const char* json);
char* lept_stringify(const lept_value* v, size_t* length);

void lept_copy(lept_value* dst, const lept_value* src);
void lept_move(lept_value* dst, lept_value* src);
void lept_swap(lept_value* lhs, lept_value* rhs);

void lept_free(lept_value* v);

lept_type lept_get_type(const lept_value* v);
int lept_is_equal(const lept_value* lhs, const lept_value* rhs);

#define lept_set_null(v) lept_free(v)

int lept_get_boolean(const lept_value* v);
void lept_set_boolean(lept_value* v, int b);

double lept_get_number(const lept_value* v);
void lept_set_number(lept_value* v, double n);

const char* lept_get_string(const lept_value* v);
size_t lept_get_string_length(const lept_value* v);
void lept_set_string(lept_value* v, const char* s, size_t len);

void lept_set_array(lept_value* v, size_t capacity);
size_t lept_get_array_size(const lept_value* v);
size_t lept_get_array_capacity(const lept_value* v);
void lept_reserve_array(lept_value* v, size_t capacity);
void lept_shrink_array(lept_value* v);
void lept_clear_array(lept_value* v);
lept_value* lept_get_array_element(lept_value* v, size_t index);
lept_value* lept_pushback_array_element(lept_value* v);
void lept_popback_array_element(lept_value* v);
lept_value* lept_insert_array_element(lept_value* v, size_t index);
void lept_erase_array_element(lept_value* v, size_t index, size_t count);

void lept_set_object(lept_value* v, size_t capacity);
size_t lept_get_object_size(const lept_value* v);
size_t lept_get_object_capacity(const lept_value* v);
void lept_reserve_object(lept_value* v, size_t capacity);
void lept_shrink_object(lept_value* v);
void lept_clear_object(lept_value* v);
const char* lept_get_object_key(const lept_value* v, size_t index);
size_t lept_get_object_key_length(const lept_value* v, size_t index);
lept_value* lept_get_object_value(lept_value* v, size_t index);
size_t lept_find_object_index(const lept_value* v, const char* key, size_t klen);
lept_value* lept_find_object_value(const lept_value* v, const char* key, size_t klen);
lept_value* lept_set_object_value(lept_value* v, const char* key, size_t klen);
void lept_remove_object_value(lept_value* v, size_t index);

#endif /* LEPTJSON_H__ */
