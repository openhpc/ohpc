/* block/test_complex_source.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 Gerard Jungman, Brian Gough
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

void FUNCTION (test, func) (void);
void FUNCTION (test, binary) (void);
void FUNCTION (test, trap) (void);

void
FUNCTION (test, func) (void)
{
  TYPE (gsl_block) * b;
  ATOMIC * data;
  size_t i, size;

  b = FUNCTION (gsl_block, alloc) (N);

  gsl_test (b->data == 0, NAME (gsl_block) "_alloc returns valid pointer");
  gsl_test (b->size != N, NAME (gsl_block) "_alloc returns valid size");

  data = FUNCTION(gsl_block, data) (b);
  size = FUNCTION(gsl_block, size) (b);

  gsl_test (data == 0, NAME (gsl_block) "_data returns valid pointer");
  gsl_test (size != N, NAME (gsl_block) "_size returns valid size");
  
  FUNCTION (gsl_block, free) (b);       /* free whatever is in v */

  b = FUNCTION (gsl_block, calloc) (N);

  gsl_test (b->data == 0, NAME (gsl_block) "_calloc returns valid pointer");
  gsl_test (b->size != N, NAME (gsl_block) "_calloc returns valid size");

  data = FUNCTION(gsl_block, data) (b);
  size = FUNCTION(gsl_block, size) (b);

  gsl_test (data == 0, NAME (gsl_block) "_data returns valid pointer from calloc");
  gsl_test (size != N, NAME (gsl_block) "_size returns valid size from calloc");

  status = 0;
  
  for (i = 0; i < N; i++)
    {
      if (b->data[2 * i] != 0.0 || b->data[2 * i + 1] != 0.0)
        status = 1;
    };

  gsl_test (status, NAME (gsl_block) "_calloc initializes array to zero");

  FUNCTION (gsl_block, free) (b);
}

void
FUNCTION (test, binary) (void)
{
  size_t i;

  {
    TYPE (gsl_block) * v = FUNCTION (gsl_block, calloc) (N);

    FILE *f = fopen ("test.dat", "wb");

    for (i = 0; i < N; i++)
      {
        v->data[2*i] = (ATOMIC)(N - i);
        v->data[2*i + 1] = (ATOMIC)(10*(N-i) + 1);
      };

    FUNCTION (gsl_block, fwrite) (f, v);

    fclose (f);
    
    FUNCTION (gsl_block, free) (v);
  }

  {
    TYPE (gsl_block) * w = FUNCTION (gsl_block, calloc) (N);

    FILE *f = fopen ("test.dat", "rb");

    FUNCTION (gsl_block, fread) (f, w);

    status = 0;
    for (i = 0; i < N; i++)
      {
        if (w->data[2 * i] != (ATOMIC) (N - i) || w->data[2 * i + 1] != (ATOMIC) (10*(N - i) + 1))
          status = 1;
      };
    fclose (f);

    FUNCTION (gsl_block, free) (w);
  }

  gsl_test (status, NAME (gsl_block) "_write and read");

}


void
FUNCTION (test, trap) (void)
{
  TYPE (gsl_block) * b = FUNCTION (gsl_block, alloc) (0);

  gsl_test (b != 0, NAME (gsl_block) "_alloc traps zero length");
}




