#include <stdio.h>
#include <stdlib.h>

/*
 * type abbreviation declaration
 */
typedef int* NEWTYPE;

/*
 * declaring functions
 */
NEWTYPE ARRAY(int n);
int STORE(NEWTYPE as, int N, int row, int col, int val);
int FETCH(NEWTYPE as, int N, int row, int col);

/*
 * new array of size n*n is created using malloc
 */
NEWTYPE ARRAY(int n) {
	return malloc(n * n * sizeof(int));
}

/*
 * integer val is stored at specified row/column
 * returns -1 if not successful
 * returns 1 if successful
 */
int STORE(NEWTYPE as, int N, int row, int col, int val) {

	if (col > N || row > N || col < 0 || row < 0) return -1; // returns -1 if row or column is out of array bound
	if ((col % 2) != (row % 2)) return -1; // returns -1 if row and column don't have the same parity

	NEWTYPE st = as;
	as += (col * N);
	as += row;
	*as = val;
	as = st;

	return 1;
}

/*
 * value is fetched from specified position
 * returns -1 if not successful
 * returns stored value if successful
 */
int FETCH(NEWTYPE as, int N, int row, int col) {

	if (col > N || row > N || col < 0 || row < 0) return -1; // returns -1 if row or column is out of array bound
	if ((col % 2) != (row % 2)) return -1; // returns -1 if row and column don't have the same parity

	NEWTYPE st = as;
	as += (col * N);	
	as += row;
	int val = *as;
	as = st;

	return val;
}
