/* block/test_io.c
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

void FUNCTION (test, text) (void);

void
FUNCTION (test, text) (void)
{
  size_t i;
  
  {
    TYPE (gsl_block) * v = FUNCTION (gsl_block, calloc) (N);

    FILE *f = fopen ("test.txt", "w");

    for (i = 0; i < N; i++)
      {
        v->data[i] = (ATOMIC) i;
      };

    FUNCTION (gsl_block, fprintf) (f, v, OUT_FORMAT);

    fclose (f);

    FUNCTION (gsl_block, free) (v);
  }

  {
    TYPE (gsl_block) * w = FUNCTION (gsl_block, calloc) (N);

    FILE *f = fopen ("test.txt", "r");

    FUNCTION (gsl_block, fscanf) (f, w);

    status = 0;
    for (i = 0; i < N; i++)
      {
        if (w->data[i] != (ATOMIC) i)
          status = 1;
      };

    gsl_test (status, NAME (gsl_block) "_fprintf and fscanf");

    fclose (f);

    FUNCTION (gsl_block, free) (w);
  }
}


