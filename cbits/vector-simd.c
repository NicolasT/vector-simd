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

//#define DEBUG

#ifdef DEBUG
# include <stdio.h>
# define MSG printf
#else
# define MSG
#endif

void _vector_simd_xor_sse42(const uint32_t *a, const uint32_t *b, const uint32_t *o, ssize_t len) {
        ssize_t i = 0,
                todo = len / 32;

        __m128 ma0, mb0, mx0,
               ma1, mb1, mx1,
               ma2, mb2, mx2,
               ma3, mb3, mx4;

        for(i = 0; i < todo; i++) {
                ma0 = _mm_load_ps((float *)a);
                mb0 = _mm_load_ps((float *)b);

                ma1 = _mm_load_ps((float *)(a + 4));
                mb1 = _mm_load_ps((float *)(b + 4));

                mx0 = _mm_xor_ps(ma0, mb0);
                _mm_store_ps((float *)o, mx0);

                mx1 = _mm_xor_ps(ma1, mb1);
                _mm_store_ps((float *)(o + 4), mx1);

                a += 8;
                b += 8;
                o += 8;
        }
}
