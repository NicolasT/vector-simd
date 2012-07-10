/* vector-simd - Combining the vector API with SIMD intrinsics
 *
 * Copyright (c) 2012 Nicolas Trangez
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This package is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdint.h>

#include <xmmintrin.h>

#define DEBUG 0
/* On my machine, adding prefetching slows things down significantly (+- 80%) */
#define DO_PREFETCH 0

#if DEBUG
# include <stdio.h>
# define MSG printf
#else
# define MSG
#endif

#define LIKELY(expr) (__builtin_expect(!!(expr), 1))


#define UPDATE_POINTERS(n) \
        do { \
                a += n; b += n; c += n; \
        } while(0)


void _vector_simd_xor_sse42(const uint8_t *a, const uint8_t *b, const uint8_t *c, ssize_t len) {

#define local_mm_load_ps(n) _mm_load_ps((float *)(&(n)))
#define local_mm_xor_ps(x, y) _mm_xor_ps(x, y)
#define local_mm_store_ps(t, s) _mm_store_ps((float *)&(t), s)

#if PREFETCH
# define local_prefetchtna(n) _mm_prefetch(&(n), _MM_HINT_NTA);
#else
# define local_prefetchnta(n)
#endif

#define UPDATE_POINTERS(n)              \
        do {                            \
                a += n; b += n; c += n; \
        } while(0)

        unsigned int cnt256 = len >> 8,
                     rest = len & 255;

        __m128 a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15,
               b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15,
               c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15;

        for(; cnt256 > 0; cnt256--) {
                asm (".align 32");

                a0 = local_mm_load_ps(a[0]);
                b0 = local_mm_load_ps(b[0]);
                local_prefetchnta(a[256]);
                local_prefetchnta(b[256]);
                a1 = local_mm_load_ps(a[16]);
                b1 = local_mm_load_ps(b[16]);
                local_prefetchnta(a[256 + 16]);
                local_prefetchnta(b[256 + 16]);
                a2 = local_mm_load_ps(a[32]);
                b2 = local_mm_load_ps(b[32]);
                local_prefetchnta(a[256 + 32]);
                local_prefetchnta(b[256 + 32]);
                a3 = local_mm_load_ps(a[48]);
                b3 = local_mm_load_ps(b[48]);
                local_prefetchnta(a[256 + 48]);
                local_prefetchnta(b[256 + 48]);
                a4 = local_mm_load_ps(a[64]);
                b4 = local_mm_load_ps(b[64]);
                local_prefetchnta(a[256 + 64]);
                local_prefetchnta(b[256 + 64]);
                a5 = local_mm_load_ps(a[80]);
                b5 = local_mm_load_ps(b[80]);
                local_prefetchnta(a[256 + 80]);
                local_prefetchnta(b[256 + 80]);
                a6 = local_mm_load_ps(a[96]);
                b6 = local_mm_load_ps(b[96]);
                local_prefetchnta(a[256 + 96]);
                local_prefetchnta(b[256 + 96]);
                a7 = local_mm_load_ps(a[112]);
                b7 = local_mm_load_ps(b[112]);
                local_prefetchnta(a[256 + 112]);
                local_prefetchnta(b[256 + 112]);
                a8 = local_mm_load_ps(a[128]);
                b8 = local_mm_load_ps(b[128]);
                local_prefetchnta(a[256 + 128]);
                local_prefetchnta(b[256 + 128]);
                a9 = local_mm_load_ps(a[144]);
                b9 = local_mm_load_ps(b[144]);
                local_prefetchnta(a[256 + 144]);
                local_prefetchnta(b[256 + 144]);
                a10 = local_mm_load_ps(a[160]);
                b10 = local_mm_load_ps(b[160]);
                local_prefetchnta(a[256 + 160]);
                local_prefetchnta(b[256 + 160]);
                a11 = local_mm_load_ps(a[176]);
                b11 = local_mm_load_ps(b[176]);
                local_prefetchnta(a[256 + 176]);
                local_prefetchnta(b[256 + 176]);
                a12 = local_mm_load_ps(a[192]);
                b12 = local_mm_load_ps(b[192]);
                local_prefetchnta(a[256 + 192]);
                local_prefetchnta(b[256 + 192]);
                a13 = local_mm_load_ps(a[208]);
                b13 = local_mm_load_ps(b[208]);
                local_prefetchnta(a[256 + 208]);
                local_prefetchnta(b[256 + 208]);
                a14 = local_mm_load_ps(a[224]);
                b14 = local_mm_load_ps(b[224]);
                local_prefetchnta(a[256 + 224]);
                local_prefetchnta(b[256 + 224]);
                a15 = local_mm_load_ps(a[240]);
                b15 = local_mm_load_ps(b[240]);
                local_prefetchnta(a[256 + 240]);
                local_prefetchnta(b[256 + 240]);

                c0 = local_mm_xor_ps(a0, b0);
                c1 = local_mm_xor_ps(a1, b1);
                c2 = local_mm_xor_ps(a2, b2);
                c3 = local_mm_xor_ps(a3, b3);
                c4 = local_mm_xor_ps(a4, b4);
                c5 = local_mm_xor_ps(a5, b5);
                c6 = local_mm_xor_ps(a6, b6);
                c7 = local_mm_xor_ps(a7, b7);
                c8 = local_mm_xor_ps(a8, b8);
                c9 = local_mm_xor_ps(a9, b9);
                c10 = local_mm_xor_ps(a10, b10);
                c11 = local_mm_xor_ps(a11, b11);
                c12 = local_mm_xor_ps(a12, b12);
                c13 = local_mm_xor_ps(a13, b13);
                c14 = local_mm_xor_ps(a14, b14);
                c15 = local_mm_xor_ps(a15, b15);

                local_mm_store_ps(c[0], c0);
                local_mm_store_ps(c[16], c1);
                local_mm_store_ps(c[32], c2);
                local_mm_store_ps(c[48], c3);
                local_mm_store_ps(c[64], c4);
                local_mm_store_ps(c[80], c5);
                local_mm_store_ps(c[96], c6);
                local_mm_store_ps(c[112], c7);
                local_mm_store_ps(c[128], c8);
                local_mm_store_ps(c[144], c9);
                local_mm_store_ps(c[160], c10);
                local_mm_store_ps(c[176], c11);
                local_mm_store_ps(c[192], c12);
                local_mm_store_ps(c[208], c13);
                local_mm_store_ps(c[224], c14);
                local_mm_store_ps(c[240], c15);

                UPDATE_POINTERS(256);
        }

        if(LIKELY(rest == 0)) {
                return;
        }

        if(rest & 128) {
                a0 = local_mm_load_ps(a[0]);
                b0 = local_mm_load_ps(b[0]);
                a1 = local_mm_load_ps(a[16]);
                b1 = local_mm_load_ps(b[16]);
                a2 = local_mm_load_ps(a[32]);
                b2 = local_mm_load_ps(b[32]);
                a3 = local_mm_load_ps(a[48]);
                b3 = local_mm_load_ps(b[48]);
                a4 = local_mm_load_ps(a[64]);
                b4 = local_mm_load_ps(b[64]);
                a5 = local_mm_load_ps(a[80]);
                b5 = local_mm_load_ps(b[80]);
                a6 = local_mm_load_ps(a[96]);
                b6 = local_mm_load_ps(b[96]);
                a7 = local_mm_load_ps(a[112]);
                b7 = local_mm_load_ps(b[112]);

                c0 = local_mm_xor_ps(a0, b0);
                c1 = local_mm_xor_ps(a1, b1);
                c2 = local_mm_xor_ps(a2, b2);
                c3 = local_mm_xor_ps(a3, b3);
                c4 = local_mm_xor_ps(a4, b4);
                c5 = local_mm_xor_ps(a5, b5);
                c6 = local_mm_xor_ps(a6, b6);
                c7 = local_mm_xor_ps(a7, b7);

                local_mm_store_ps(c[0], c0);
                local_mm_store_ps(c[16], c1);
                local_mm_store_ps(c[32], c2);
                local_mm_store_ps(c[48], c3);
                local_mm_store_ps(c[64], c4);
                local_mm_store_ps(c[80], c5);
                local_mm_store_ps(c[96], c6);
                local_mm_store_ps(c[112], c7);

                rest ^= 128;
                if(rest == 0) {
                        return;
                }

                UPDATE_POINTERS(128);
        }

        if(rest & 64) {
                a0 = local_mm_load_ps(a[0]);
                b0 = local_mm_load_ps(b[0]);
                a1 = local_mm_load_ps(a[16]);
                b1 = local_mm_load_ps(b[16]);
                a2 = local_mm_load_ps(a[32]);
                b2 = local_mm_load_ps(b[32]);
                a3 = local_mm_load_ps(a[48]);
                b3 = local_mm_load_ps(b[48]);

                c0 = local_mm_xor_ps(a0, b0);
                c1 = local_mm_xor_ps(a1, b1);
                c2 = local_mm_xor_ps(a2, b2);
                c3 = local_mm_xor_ps(a3, b3);

                local_mm_store_ps(c[0], c0);
                local_mm_store_ps(c[16], c1);
                local_mm_store_ps(c[32], c2);
                local_mm_store_ps(c[48], c3);

                rest ^= 64;
                if(rest == 0) {
                        return;
                }

                UPDATE_POINTERS(64);
        }

        if(rest & 32) {
                a0 = local_mm_load_ps(a[0]);
                b0 = local_mm_load_ps(b[0]);
                a1 = local_mm_load_ps(a[16]);
                b1 = local_mm_load_ps(b[16]);

                c0 = local_mm_xor_ps(a0, b0);
                c1 = local_mm_xor_ps(a1, b1);

                local_mm_store_ps(c[0], c0);
                local_mm_store_ps(c[16], c1);

                rest ^= 32;
                if(rest == 0) {
                        return;
                }

                UPDATE_POINTERS(32);
        }

        if(rest & 16) {
                a0 = local_mm_load_ps(a[0]);
                b0 = local_mm_load_ps(b[0]);

                c0 = local_mm_xor_ps(a0, b0);

                local_mm_store_ps(c[0], c0);

                /* For consistency, gets thrown away by compiler */
                UPDATE_POINTERS(16);
        }

#undef local_mm_load_ps
#undef local_mm_xor_ps
#undef local_mm_store_ps
#undef UPDATE_POINTERS

}
