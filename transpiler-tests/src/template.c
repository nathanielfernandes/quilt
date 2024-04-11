#include <stdio.h>
#include <stdlib.h>

#define bool _Bool
#define i64 long long int

typedef struct Value {
  // Define the tag for different types
  enum { INT, BOOL, STRING } type;

  // Define the union that can hold different types of values
  union {
    bool boolValue;
    i64 intValue;
    char *stringValue;
  } value;
} Value;

Value *ALLOCATE(Value value) {
  Value *v = (Value *)malloc(sizeof(Value));
  *v = value;
  return v;
}

#define INT(x)                                                                 \
  (Value) { .type = INT, .value.intValue = x }
#define BOOL(x)                                                                \
  (Value) { .type = BOOL, .value.boolValue = x }
#define STRING(x)                                                              \
  (Value) { .type = STRING, .value.stringValue = x }

void println(Value *value) {
  switch (value->type) {
  case INT:
    printf("%lld\n", value->value.intValue);
    break;
  case BOOL:
    printf("%s\n", value->value.boolValue ? "true" : "false");
    break;
  case STRING:
    printf("%s\n", value->value.stringValue);
    break;
  }
}

/////////////////

//_signatures_//

/////////////////

//_definitions_//

/////////////////

int main() {
  //_main_//
  return 0;
}