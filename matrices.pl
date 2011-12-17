% First saved: 10/03/2011
% Last saved: 10/03/2011
%
% Doing:
%
% Todo
%
% NOTES:
/*
	A matrix :
	A =	[a, 	bcd, 	ef	]
		[123, 1, 	1234	]

	Cab be represented as a list:

	In row-major order,
	- flat (row-to-row) :
	  Matrix_rmo_f = [a, bcd, ef, 123, 1, 1234]
	 -and expanded (each sublist is a row):
	  Matrix_rmo_x = [[a, bcd, ef],[123, 1, 1234]]

	In column-major order,
	- flat (column-to-column:
	 Matrix_cmo_f = [a,123,bcd,1,ef,1234]
	- and expanded (each sublist is a column):
	 Matrix_cmo_x = [[a, 123], [bcd, 1], [ef, 1234]]

	Predicates in this file assume the row-major, expanded
	  form of the matrix is known- ie, a table.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%         Swi Compatibility         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- set_prolog_flag(backquoted_string,true).
	% Swi LPA backtic string compatibility.
	:- style_check(-singleton).
	% Stop Swi warning on singleton vars.
	:- style_check(-discontiguous).
	% Stop Swi warning on discontiguous clauses.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Matrices             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%columns_x_rows/3 10/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* columns_x_rows(+Matrix, -Columns, -Rows) */
	% Returns the number of Columns and Rows of a Matrix
	% The Matrix is represented as a nested list; each sublist is a row.

	% A matrix: a list of n-element lists.
	% Report its number of columns by its number of rows
	columns_x_rows(Matrix, Columns, Rows):-
		member(Row, Matrix),
		len(Row, Columns),
		len(Matrix, Rows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%columns/2 10/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* columns(+Matrix, -Columns */
	% Expands a matrix to its column-major order list form.
	% Top-level goal for columns/5

	columns(Matrix, Columns):-
		columns_x_rows(Matrix, Cols, _Rows),
		columns(Matrix, 1, Cols, [], Columns).


%%%%%%%%%%%%%%columns/5 10/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* columns(+Matrix, -Position, No_of_columns, [], -Expanded_columns) */
	% Matrix: an expanded row-major order matrix;
	% Columns: the expanded, column-major order of Matrix

%%%%%%%%%%%%%%columns/5 (0) 10/03/11

	columns(_Matrix, Position, Cols, Columns, Columns):-
			Position is Cols + 1.

%%%%%%%%%%%%%%columns/5 (1) 10/03/11

	columns(Matrix, Position, Cols, Temp, Columns):-
		Position =< Cols,
		findall(Element,
				(member(Row, Matrix),
				member(Element, Row, Position)),
			One_column),
		New_Position is Position + 1,
		append(Temp, [One_column], New_temp),
		columns(Matrix, New_Position, Cols, New_temp, Columns) .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%column_major_order/2 10/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* column_major_order(+Matrix, -Reordered) */
	% Rearranges the expanded form of a row-major order Matrix
	%  as a flat list of its elements in column-major order .

	column_major_order(Matrix, Reordered):-
		findall(Col_no - Element,
%			Row_no ^ (member(Row, Matrix, Row_no),
%			Swi, 19/06/11
			(member(Row, Matrix),
			member(Element, Row, Col_no)),
		Ordered),
		keysort(Ordered, Keysorted),
		findall(Element,
%				Col_no ^ member(Col_no - Element , Keysorted),
%				Swi, 19/06/11
				member(Col_no - Element , Keysorted),
			Reordered).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Use flatten_list/2 flatten a row-major order list form of a matrix
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%element_offset_rm/3 10/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* element_offset_rm(+Matrix, ?Offset, ?Element) */
	% Returns the Element at Offset of a Matrix listed in
	%  row-major order (expanded)

	% Offset = row number x number of columns + column number
	element_offset_rm(Matrix, Offset, Element):-
		columns_x_rows(Matrix, Cols, _Rows),
		member(Row, Matrix, Row_no), % green cut?
		member(Element, Row, Col_no),
		Rnum is Row_no - 1,
		Colnum is Col_no - 1, % Adjust to counting from 0
		Off_set is Rnum * Cols + Colnum,
		Offset is Off_set + 1. % Readjust to counting from 1


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%element_offset_rm/3 10/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* element_offset_cm(+Matrix, ?Offset, ?Element) */
	% Returns the Element at Offset of a Matrix listed in
	%  column-major order (expanded)

	% Offset = column number x number of rows  + row number.
	element_offset_cm(Matrix, Offset, Element):-
		columns_x_rows(Matrix, Rows, _Cols), % !, green cut
		% ^ Arguments inverted to represent column-major order!
		member(Column, Matrix, Col_no),
		member(Element, Column, Row_no),
		Rnum is Row_no - 1,
		Colnum is Col_no - 1, % Adjust to counting from 0
		 Off_set is Colnum * Rows + Rnum,
		Offset is Off_set + 1. % Readjust to counting from 1








