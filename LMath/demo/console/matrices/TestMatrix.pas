program TestMatrix;
uses uTypes, uErrors, uMatrix, uVecUtils, uVectorHelper, uVecMatPrn, sysutils, dateutils;
const
  LB = 0;

var
  M1, M2, M3, M4, Res : TMatrix;
  ResV, ResV2, Resv3: TVector;
  Rows1, Cols1, Cols2, I : integer;
  time1, time2 : TDateTime;
  ResF: float;
begin
  {%region Initializing matrices}
  Rows1 := LB+1; Cols1 := LB+2; // Ro* is highest index of rows and Col* is highest index of columns
  Cols2 := LB+2;
  vprnLB := LB;
  DimMatrix(M1, Rows1, Cols1);
  DimMatrix(M2, Cols1, Cols2);

  for I := Lb to Rows1 do
    M1[I] := Seq(Lb,Cols1,I,2,M1[I]);
  M2[LB].FillWithArr(Lb, [-5,8,11]);
  M2[LB+1].FillWithArr(Lb,[3,9,21]);
  M2[LB+2].FillWithArr(LB,[4,0,8]);
  {%endregion}
  {%region Multiply Matrix with vector and matrix with matrix}
  writeln('Matrix with vector multiplication.');
  writeln('Matrix:');
  PrintMatrix(M2);
  writeln('Vector:');
  printVector(M1[1][0..High(M1[1])]);
  ResV := MatVecMul(M2,M1[1],LB);
  writeln('result:');
  PrintVector(ResV[0..High(ResV)]);
  writeln('Multiplying matrix:');
  printMatrix(M1);
  writeln('with:');
  printMatrix(M2);
  Res := MatMul(M1,M2,Lb);
  writeln('Result:');
  PrintMatrix(Res);
  {%endregion}
  {%region Timing}
  //writeln('Now testing matrix multiplication timing.');
  //writeln('First, multiply same matrices 1000000 times.');
  //writeln('Press [enter] to begin test.');
  //readln;
  //time1 := time;
  //DimMatrix(Res,Rows1,Cols2);
  //for I := 1 to 1000000 do
  //  MatMul(M1, M2, LB, Res);
  //finalize(res);
  //time2 := time;
  //writeln('it takes ',inttostr(millisecondsbetween(time2, time1)), ' ms.');
  //
  //write('Now big matrices, 1500x1500...');
  //Rows1 := LB+1500; Cols1 := Rows1; Cols2 := Rows1;
  //DimMatrix(M3, Rows1, Cols1);
  //DimMatrix(M4, Cols1, Cols2);
  //time1 := time;
  //DimMatrix(Res,Rows1,Cols2);
  //MatMul(M3,M4,LB,Res);
  //
  //time2 := time;
  //writeln('it takes ',inttostr(millisecondsbetween(time2, time1)), ' ms.');
  //write('Press [Enter] to continue...');
  //readln;
 {%endregion}
  {%region Transpose and TransposeInPlace}
  Rows1 := LB+1; Cols1 := LB+2;
  writeln('Test of transpose.');
  DimMatrix(Res, Cols1, Rows1);
  matTranspose(M1,LB,Res);
  writeln('initial:');
  Printmatrix(M1);
  writeln('Transposed:');
  printMatrix(Res);
  writeln('Test for transpose in place.');
  write('Press [Enter] to continue...');
  readln;
  DimMatrix(M3,5,5);
  for I := 0 to 5 do
    Seq(0,5,I*10,1,M3[I]);
  writeln('Original matrix:');
  writeln('Press [Enter] to continue...');
  PrintMatrix(M3);
  MatTransposeInPlace(M3,0,High(M3));
  writeln('Transposed matrix:');
  PrintMatrix(M3);
  {%endregion}
  {%region Vector with Float and Matrix with Float operators}
  writeln('Operators Vector with Float and Matrix with Float:');
  write('Press [Enter] to continue...');
  readln;
  printvector(ResV[0..High(ResV)]);
  writeln('and matrix');
  printmatrix(M1);
  write('Press [Enter] to continue...');
  readln;
  writeln('+4');
  ResV2 := ResV + 4;
  printvector(ResV2[0..High(ResV2)]);
  writeln;
  Res := M1+4;
  printmatrix(Res);

  writeln('-4');
  ResV2 := ResV-4;
  printvector(ResV2[0..High(ResV2)]);
  writeln;
  Res := M1-4;
  printmatrix(Res);

  writeln('*2');
  ResV2 := ResV*2;
  printvector(ResV2[0..High(ResV2)]);
  writeln;
  Res := M1*2;
  printmatrix(Res);

  writeln('/8');
  ResV2 := ResV/8;
  printvector(ResV2[0..High(ResV2)]);
  writeln;
  Res := M1/8;
  printmatrix(Res);
  write('Press [Enter] to continue...');
  readln;
  {%endregion}
  {%region Vector and float functions}
  write('Vector and float function calls.');
  writeln('Add');
  VecFloatAdd(ResV[0..High(ResV)],4,ResV2[0..High(ResV)]);
  printvector(ResV2[0..High(ResV)]);
  write('Press [Enter] to continue...');
  readln;
  {%endregion}
  {%region Vector and Vector operators}
  writeln('Elemental operators with vectors.');
  writeln('Testing operations on vector sections');
  ResV := Seq(1,8,1,1);
  ResV2 := Seq(1,8,10,1);
  writeln('First vector:');
  printVector(ResV[1..high(ResV)]);
  writeln('Second Vector');
  printVector(ResV2[1..high(ResV2)]);
  writeln('Now add elements 2 to 6 from first and 4 to 8 of second');
  Resv3 := ResV[2..6]+ResV2[4..8];
  printVector(ResV3[0..high(ResV3)]);
  readln;
  writeln('Now vector:');
  printVector(ResV);
  writeln('/9.5');
  resv := resv/9.5;
  printVector(ResV);
  writeln('This result + ');
  printvector(ResV2);
  ResV3 := ResV+ResV2;
  printvector(ResV3);
  writeln('Now');
  printVector(ResV);
  writeln('-');
  printvector(ResV2);
  ResV3 := ResV-ResV2;
  printvector(ResV3);
  {%endregion}
  writeln('multiply with VecElemMul and open arrays.');
  printVector(ResV);
  printvector(ResV2);
  VecElemMul(ResV,ResV2,ResV3);
  printvector(ResV3);
  readln;
  ResV := Seq(0,8,1,1);
  writeln('Dot product of:');
  printVector(ResV);
  writeln('and');
  printvector(ResV2);
  ResF := VecDotProd(ResV,Resv2,Lb, high(ResV));
  writeln(Format(vprnFmtStr,[ResF]));
  ResF := VecDotProd(ResV,ResV2);
  writeln('or: ',Format(vprnFmtStr,[ResF]));
  writeln('Now testing outer product.');
  ResV := TVector.Create(0,2.0,3.0,4.0,5.0);
  ResV2 := TVector.Create(0,3.0,4.0,5.0);
  PrintVector(ResV);
  writeln('by');
  PrintVector(ResV2);
  writeln('Result:');
  M3 := VecOuterProd(ResV,ResV2);
  PrintMatrix(M3);
  writeln('Now opposite order');
  M3 := VecOuterProd(ResV2,ResV);
  PrintMatrix(M3);

  write('Press [Enter] to terminate...');
  readln;
end.

