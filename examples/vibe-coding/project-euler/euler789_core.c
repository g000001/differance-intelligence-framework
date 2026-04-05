
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define P 2000000011ULL

typedef struct {
    uint32_t mod_val;
    uint32_t weight;
    uint64_t val;
} Item;

static uint64_t mod_inv(uint64_t a, uint64_t m) {
    int64_t m0 = m, y = 0, x = 1;
    int64_t a0 = a;
    if (m == 1) return 0;
    while (a0 > 1) {
        int64_t q = a0 / m0;
        int64_t t = m0;
        m0 = a0 % m0;
        a0 = t;
        t = y;
        y = x - q * y;
        x = t;
    }
    if (x < 0) x += m;
    return x;
}

static const uint64_t primes[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 
    61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139
};
static const int num_primes = sizeof(primes) / sizeof(primes[0]);

// 動的配列管理
static Item* list_a = NULL;
static size_t a_count = 0;
static size_t a_capacity = 0;

static void push_item(uint32_t m, uint32_t w, uint64_t v) {
    if (a_count >= a_capacity) {
        a_capacity = (a_capacity == 0) ? 1000000 : a_capacity * 2;
        list_a = (Item*)realloc(list_a, a_capacity * sizeof(Item));
        if (!list_a) {
            fprintf(stderr, "Memory allocation failed!\n");
            exit(1);
        }
    }
    list_a[a_count].mod_val = m;
    list_a[a_count].weight = w;
    list_a[a_count].val = v;
    a_count++;
}

static void dfs_a(int idx, uint32_t w, uint64_t v, uint64_t m, uint32_t max_w) {
    uint64_t inv = mod_inv(m, P);
    uint64_t target = (P - inv) % P;
    
    push_item((uint32_t)target, w, v);

    for (int i = idx; i < num_primes; i++) {
        uint32_t pw = primes[i] - 1;
        if (w + pw <= max_w) {
            dfs_a(i, w + pw, v * primes[i], (m * primes[i]) % P, max_w);
        }
    }
}

int compare_items(const void* a, const void* b) {
    uint32_t ma = ((Item*)a)->mod_val;
    uint32_t mb = ((Item*)b)->mod_val;
    if (ma < mb) return -1;
    if (ma > mb) return 1;
    return 0;
}

static uint32_t min_w = 1000000;
static uint64_t best_product = 0;
static size_t compressed_count = 0;

static void dfs_b(int idx, uint32_t w, uint64_t v, uint64_t m, uint32_t max_w) {
    long long left = 0, right = (long long)compressed_count - 1;
    while (left <= right) {
        long long mid = left + (right - left) / 2;
        uint32_t mid_m = list_a[mid].mod_val;
        if (mid_m == (uint32_t)m) {
            uint32_t total_w = w + list_a[mid].weight;
            if (total_w < min_w) {
                min_w = total_w;
                best_product = v * list_a[mid].val;
            }
            break;
        } else if (mid_m < (uint32_t)m) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    for (int i = idx; i < num_primes; i++) {
        uint32_t pw = primes[i] - 1;
        uint32_t next_w = w + pw;
        if (next_w < min_w && next_w <= max_w) {
            dfs_b(i, next_w, v * primes[i], (m * primes[i]) % P, max_w);
        }
    }
}

uint64_t solve_p789_c() {
    uint32_t w_a_max = 110;
    uint32_t w_b_max = 140;

    a_count = 0;
    a_capacity = 0;
    if (list_a) { free(list_a); list_a = NULL; }
    min_w = 1000000;
    best_product = 0;

    dfs_a(0, 0, 1, 1, w_a_max);
    
    qsort(list_a, a_count, sizeof(Item), compare_items);

    compressed_count = 0;
    uint32_t prev_m = 0xFFFFFFFF;
    for (size_t i = 0; i < a_count; i++) {
        if (list_a[i].mod_val != prev_m) {
            list_a[compressed_count] = list_a[i];
            prev_m = list_a[i].mod_val;
            compressed_count++;
        } else {
            if (list_a[i].weight < list_a[compressed_count - 1].weight) {
                list_a[compressed_count - 1].weight = list_a[i].weight;
                list_a[compressed_count - 1].val = list_a[i].val;
            }
        }
    }

    dfs_b(0, 0, 1, 1, w_b_max);

    if (list_a) { free(list_a); list_a = NULL; }
    return best_product;
}
