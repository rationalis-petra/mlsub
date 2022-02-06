#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "hashmap/hashmap.h"

// IO handle
int64_t printi(int64_t i) {
  printf("%ld\n", i);
  return 0;
}
// Hashmap helpers

struct entry {
  int64_t key;
  int64_t val;
};

int entry_compare(const void *a, const void *b, void *udata) {
    const struct entry *ua = a;
    const struct entry *ub = b;
    return ua->key - ub->key;
}


uint64_t entry_hash(const void *item, uint64_t seed0, uint64_t seed1) {
    const struct entry *entry = item;
    return (uint64_t) entry->key;
}


// Hasmap functions bound by the language

int64_t mk_record(size_t capacity) {
  struct hashmap *record = hashmap_new(sizeof(struct entry),
                                       capacity, 0, 0,
                                       entry_hash, entry_compare,
                                       NULL, NULL);
  return (int64_t) record;
}

void set_field(int64_t map, int64_t _key, int64_t _val) {
  struct hashmap* record = (struct hashmap*) map;
  hashmap_set(record, &(struct entry){ .key=_key, .val=_val});
}

// helper function used as part of closure
int64_t get_field(int64_t map, int64_t* env) {
  struct hashmap *record = (struct hashmap *) map;
  struct entry* entry;
  int64_t my_key = *env;
  entry = hashmap_get(record, &(struct entry){ .key=my_key });
  return entry->val;
}

// function which generates a closure to access a record
int64_t mk_field_access(int64_t key) {
  int64_t* closure = malloc(2 * sizeof(int64_t));
  closure[0] = (int64_t) get_field;
  closure[1] = key;
  return (int64_t) closure;
}
