#include <stdatomic.h>
#include <stdint.h>
#include <stdbool.h>

#pragma warning(disable:4100)   // Unused formal parameters.

// Workaround for MSVC bug
// https://developercommunity.visualstudio.com/t/Using-atomic_load_explicit-with-an-unsig/10414053?q=atomic_load_explicit+c4244
#pragma warning(push)
#pragma warning(disable:4244)
uint8_t __atomic_load_1(uint8_t *src, int model) {
    return atomic_load_explicit((atomic_uchar *)src, model);
}

uint16_t __atomic_load_2(uint16_t *src, int model) {
    return atomic_load_explicit((atomic_ushort *)src, model);
}
#pragma warning(pop)

uint32_t __atomic_load_4(uint32_t *src, int model) {
    return atomic_load_explicit((atomic_uint *)src, model);
}

uint64_t __atomic_load_8(uint64_t *src, int model) {
    return atomic_load_explicit((atomic_ullong *)src, model);
}

void __atomic_store_1(uint8_t *dest, uint8_t val, int model) {
    atomic_store_explicit((atomic_uchar *)dest, val, model);
}

void __atomic_store_2(uint16_t *dest, uint16_t val, int model) {
    atomic_store_explicit((atomic_ushort *)dest, val, model);
}

void __atomic_store_4(uint32_t *dest, uint32_t val, int model) {
    atomic_store_explicit((atomic_uint *)dest, val, model);
}

void __atomic_store_8(uint64_t *dest, uint64_t val, int model) {
    atomic_store_explicit((atomic_ullong *)dest, val, model);
}

uint8_t __atomic_exchange_1(uint8_t *dest, uint8_t val, int model) {
    return atomic_exchange_explicit((atomic_uchar *)dest, val, model);
}

uint16_t __atomic_exchange_2(uint16_t *dest, uint16_t val, int model) {
    return atomic_exchange_explicit((atomic_ushort *)dest, val, model);
}

uint32_t __atomic_exchange_4(uint32_t *dest, uint32_t val, int model) {
    return atomic_exchange_explicit((atomic_uint *)dest, val, model);
}

uint64_t __atomic_exchange_8(uint64_t *dest, uint64_t val, int model) {
    return atomic_exchange_explicit((atomic_ullong *)dest, val, model);
}

bool __atomic_compare_exchange_1(uint8_t *ptr, uint8_t *expected,
                                 uint8_t desired, int success, int failure) {
    return atomic_compare_exchange_strong_explicit((atomic_uchar *)ptr, expected, desired, success, failure);
}

bool __atomic_compare_exchange_2(uint16_t *ptr, uint16_t *expected,
                                 uint16_t desired, int success, int failure) {
    return atomic_compare_exchange_strong_explicit((atomic_ushort *)ptr, expected, desired, success, failure);
}

bool __atomic_compare_exchange_4(uint32_t *ptr, uint32_t *expected,
                                 uint32_t desired, int success, int failure) {
    return atomic_compare_exchange_strong_explicit((atomic_uint *)ptr, expected, desired, success, failure);
}

bool __atomic_compare_exchange_8(uint64_t *ptr, uint64_t *expected,
                                 uint64_t desired, int success, int failure) {
    return atomic_compare_exchange_strong_explicit((atomic_ullong *)ptr, expected, desired, success, failure);
}

uint8_t __atomic_fetch_add_1(uint8_t *ptr, uint8_t val, int model) {
    return atomic_fetch_add_explicit((atomic_uchar *)ptr, val, model);
}

uint16_t __atomic_fetch_add_2(uint16_t *ptr, uint16_t val, int model) {
    return atomic_fetch_add_explicit((atomic_ushort *)ptr, val, model);
}

uint32_t __atomic_fetch_add_4(uint32_t *ptr, uint32_t val, int model) {
    return atomic_fetch_add_explicit((atomic_uint *)ptr, val, model);
}

uint64_t __atomic_fetch_add_8(uint64_t *ptr, uint64_t val, int model) {
    return atomic_fetch_add_explicit((atomic_ullong *)ptr, val, model);
}

uint8_t __atomic_fetch_sub_1(uint8_t *ptr, uint8_t val, int model) {
    return atomic_fetch_sub_explicit((atomic_uchar *)ptr, val, model);
}

uint16_t __atomic_fetch_sub_2(uint16_t *ptr, uint16_t val, int model) {
    return atomic_fetch_sub_explicit((atomic_ushort *)ptr, val, model);
}

uint32_t __atomic_fetch_sub_4(uint32_t *ptr, uint32_t val, int model) {
    return atomic_fetch_sub_explicit((atomic_uint *)ptr, val, model);
}

uint64_t __atomic_fetch_sub_8(uint64_t *ptr, uint64_t val, int model) {
    return atomic_fetch_sub_explicit((atomic_ullong *)ptr, val, model);
}

uint8_t __atomic_fetch_and_1(uint8_t *ptr, uint8_t val, int model) {
    return atomic_fetch_and_explicit((atomic_uchar *)ptr, val, model);
}

uint16_t __atomic_fetch_and_2(uint16_t *ptr, uint16_t val, int model) {
    return atomic_fetch_and_explicit((atomic_ushort *)ptr, val, model);
}

uint32_t __atomic_fetch_and_4(uint32_t *ptr, uint32_t val, int model) {
    return atomic_fetch_and_explicit((atomic_uint *)ptr, val, model);
}

uint64_t __atomic_fetch_and_8(uint64_t *ptr, uint64_t val, int model) {
    return atomic_fetch_and_explicit((atomic_ullong *)ptr, val, model);
}

uint8_t __atomic_fetch_or_1(uint8_t *ptr, uint8_t val, int model) {
    return atomic_fetch_or_explicit((atomic_uchar *)ptr, val, model);
}

uint16_t __atomic_fetch_or_2(uint16_t *ptr, uint16_t val, int model) {
    return atomic_fetch_or_explicit((atomic_ushort *)ptr, val, model);
}

uint32_t __atomic_fetch_or_4(uint32_t *ptr, uint32_t val, int model) {
    return atomic_fetch_or_explicit((atomic_uint *)ptr, val, model);
}

uint64_t __atomic_fetch_or_8(uint64_t *ptr, uint64_t val, int model) {
    return atomic_fetch_or_explicit((atomic_ullong *)ptr, val, model);
}

uint8_t __atomic_fetch_xor_1(uint8_t *ptr, uint8_t val, int model) {
    return atomic_fetch_xor_explicit((atomic_uchar *)ptr, val, model);
}

uint16_t __atomic_fetch_xor_2(uint16_t *ptr, uint16_t val, int model) {
    return atomic_fetch_xor_explicit((atomic_ushort *)ptr, val, model);
}

uint32_t __atomic_fetch_xor_4(uint32_t *ptr, uint32_t val, int model) {
    return atomic_fetch_xor_explicit((atomic_uint *)ptr, val, model);
}

uint64_t __atomic_fetch_xor_8(uint64_t *ptr, uint64_t val, int model) {
    return atomic_fetch_xor_explicit((atomic_ullong *)ptr, val, model);
}
