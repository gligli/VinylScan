{   Pascal translation by V.V. Nesterov from FORTRAN 77 subroutine written
    by Prof. M.Powell. This is relatively crude translation with most of 
    GoTo statements. Matrices in the calculations are dealt with fortran
    order "columns first". So, there is place for optimization. If somebody
    endeavors this, please let me know.
    
 This procedure calculates an N-component vector DX by applying the
following two stages. In the first stage DX is set to the shortest
vector that minimizes the greatest violation of the constraints
  A[1,K]*DX[1]+A[2,K]*DX[2]+...+A[N,K]*DX[N] >= B[K], K := 2,3,...,M,
subject to the Euclidean length of DX being at most RHO. If its length is
strictly less than RHO, then we use the resultant freedom in DX to
minimize the objective function
         -A[1,M+1]*DX[1]-A[2,M+1]*DX[2]-...-A[N,M+1]*DX[N]
subject to no increase in any greatest constraint violation. This
notation allows the gradient of the objective function to be regarded as
the gradient of a constraint. Therefore the two stages are distinguished
by MCON = M and MCON > M respectively. It is possible that a
degeneracy may prevent DX from attaining the target length RHO. Then the
value IFULL = 0 would be set, but usually IFULL = 1 on return.

In general NACT is the number of constraints in the active set and
IACT[1],...,IACT[NACT] are their indices, while the remainder of IACT
contains a permutation of the remaining constraint indices. Further, Z is
an orthogonal matrix whose first NACT columns can be regarded as the
result of Gram-Schmidt applied to the active constraint gradients. For
J := 1,2,...,NACT, the number ZDOTA[J] is the scalar product of the J-th
column of Z with the gradient of the J-th active constraint. DX is the
current vector of variables and here the residuals of the active
constraints should be zero. Further, the active constraints have
nonnegative Lagrange mulpliers that are held at the beginning of
VMUltc. The remainder of this vector holds the residuals of the inactive
constraints at DX, the ordering of the components of vmultc being in
agreement with the permutation of the indices of the constraints that is
in IACT. All these residuals are nonnegative, which is achieved by the
shift RESMAX that makes the lest residual zero. }

unit uTrsTlp;
interface
uses uTypes, uMinMax, uMath, uMatrix, uVectorHelper, uTrigo;

procedure TrsTlp(N, M : integer; A : TMatrix; B : TVector; RHO : float; DX : TVector; out IFULL : integer);


implementation
var
  IAct : TIntVector;
  Z : TMatrix;
  ZDota, VMultc, SDirn, DXNew, VMultd : TVector;

procedure InitTrsTlp(M: integer; N: integer; DX : TVector);
var
  i: Integer;
begin
  DimVector(IAct,m+1);
  DimMatrix(Z, N,N);
  DimVector(ZDota,N);
  DimVector(VMultC,M+1);
  DimVector(SDirn,N);
  DimVector(DXNew, N);
  DimVector(VMultD, M+1);
  for i := 1 to N do
  begin
    z[i].Fill(0,N,0);
    z[i,i] := 1.0;
  end;
  DX.Fill(1,N,0);
end;

procedure FinTrsTlp;
begin
  Finalize(IAct);
  Finalize(Z);
  Finalize(ZDota);
  Finalize(VMultC);
  Finalize(SDirn);
  Finalize(DXNew);
  Finalize(VMultD);
end;

procedure TrsTlp(N, M : integer; A : TMatrix; B : TVector; RHO : float; DX : TVector; out IFULL : integer);

var
  mcon, nact, I, K, ICon , icount, nactx, kk, kp, kw, isave, kl: integer;
  resmax, temp, tempa, vsave, dd, resold, sum: float;
  optold, optnew, tot, sp, spabs, acca, accb, alpha, beta, ratio, zdotv, zdvabs, sd, ss, stpful, step, zdotw,
        zdwabs, sumabs: float;

label
  60, 70, 130, 210, 260, 320, 340, 390, 480, 490;


  {  Initialize Z and some other variable. The value of RESMAX will be
    appropriate to DX = 0, while ICON will be the index of a most violated
    constraint if RESMAX is positive. Usually during the first stage the
    vector SDIRN gives a search direction that reduces all the active
    constraint violations by one simultaneously.  }
begin
      InitTrsTlp(M,N,DX);
      ifull := 1;
      mcon := m; // first stage, dealing with constraints
      nact := 0;
      resmax := 0.0; // maximal violation
      if m >= 1 then
      begin
        for k := 1 to m do
        if b[k] > resmax then
        begin
            resmax := b[k];  // resmax set to maximal constraint violation
            icon := k;       // icon is index maximally violated constraint
        end;
        for k := 1 to m do
        begin
          iact[k] := k;      // for now, constraints are in initial order
          vmultc[k] := resmax-b[k];
        end;
      end;
      if resmax = 0.0 then goto 480; // no violations. first stage over
      sdirn.Clear;
//
//     End the current stage of the calculation if 3 consecutive iterations
//     have either failed to reduce the best calculated value of the objective
//     function or to increase the number of active constraints since the best
//     value was calculated. This strategy prevents cycling, but there is a
//     remote possibility that it will cause premature termination.
//
60:   optold := 0.0;
      icount := 0;
70:   if mcon = m then     // first stage, there are violated constraints
        optnew := resmax
      else begin
        optnew := 0.0;    // second stage, constraints are OK
        for i := 1 to n do
          optnew := optnew-dx[i]*A[i,mcon];
      end;
      if (icount = 0) or (optnew < optold) then
      begin
        optold := optnew;
        nactx := nact;
        icount := 3;
      end else if nact > nactx then
      begin
        nactx := nact;
        icount := 3;
      end else
      begin
        icount := icount-1;
        if icount = 0 then goto 490;
      end;
//
//     If ICON exceeds NACT, then we add the constraint with index IACT[ICON] to
//     the active set. Apply Givens rotations so that the last N-NACT-1 columns
//     of Z are orthogonal to the gradient of the new constraint, a scalar
//     product being set to zero if its nonzero value could be due to computer
//     rounding errors. The array DXNEW is used for working space.
//
      if icon <= nact then goto 260;
      kk := iact[icon]; // iact is array of active constrains
      for i := 1 to n do
        dxnew[i] := A[i,kk];  // extract column for maximally violated constrain
      tot := 0.0;
      k := n;
      while k > nact do
      begin
        sp := 0.0;
        spabs := 0.0;
        for i := 1 to n do
        begin
          temp := z[i,k]*dxnew[i]; // z is initially identity matrix
          sp := sp+temp;
          spabs := spabs+abs(temp);
        end;
        acca := spabs+0.1*abs(sp);
        accb := spabs+0.2*abs(sp);
        if (spabs >= acca) or (acca >= accb) then
           sp := 0.0;
        if IsZero(Tot) then
          tot := sp
        else begin
          kp := k+1;
          temp := Pythag(sp,tot); // sqrt(sp*sp+tot*tot);
          alpha := sp/temp;
          beta := tot/temp;
          tot := temp;
          for i := 1 to n do
          begin
            temp := alpha*z[i,k]+beta*z[i,kp];
            z[i,kp] := alpha*z[i,kp]-beta*z[i,k];
            z[i,k] := temp;
          end;
        end;
        k := k-1;
      end;
//
//     Add the new constraint if this can be done without a deletion from the
//     active set.
//
      if not IsZero(tot) then
      begin
        inc(nact);
        zdota[nact] := tot;
        vmultc[icon] := vmultc[nact];
        vmultc[nact] := 0.0;
        goto 210;
      end;
//
//     The next instruction is reached if a deletion has to be made from the
//     active set in order to make room for the new active constraint, because
//     the new constraint gradient is a linear combination of the gradients of
//     the old active constraints. Set the components of vmultc to the multipliers
//     of the linear combination. Further, branch if no suitable index can be found.
//
      ratio := -1.0;
      k := nact;
130:  zdotv := 0.0;
      zdvabs := 0.0;
      for i := 1 to n do
      begin
        temp := z[i,k]*dxnew[i];
        zdotv := zdotv+temp;
        zdvabs := zdvabs+abs(temp);
      end;
      acca := zdvabs+0.1*abs(zdotv);
      accb := zdvabs+0.2*abs(zdotv);
      if (zdvabs < acca) and (acca < accb) then
      begin
        temp := zdotv/zdota[k];
        if (temp > 0.0) and (iact[k] <= m) then
        begin
          tempa := vmultc[k]/temp;
          if (ratio < 0.0) or (tempa < ratio) then
            ratio := tempa;
        end;
        if (k >= 2) then
        begin
          kw := iact[k];
          for i := 1 to n do
            dxnew[i] := dxnew[i]-temp*A[i,kw];
        end;
        vmultd[k] := temp;
      end else
        vmultd[k] := 0.0;
      k := k-1;
      if (k > 0) then
        goto 130;
      if (ratio < 0.0) then
        goto 490;
//
//     Revise the Lagrange multipliers and reorder the active constraints so
//     that the one to be replaced is at the end of the list. Also calculate the
//     new value of ZDOTA[NACT] and branch if it is not acceptable
//
      for k := 1 to nact do
        vmultc[k] := Max(0.0,vmultc[k]-ratio*vmultd[k]);
      if (icon < nact) then
      begin
        isave := iact[icon];
        vsave := vmultc[icon];
        k := icon;
        repeat
          kp := k+1;
          kw := iact[kp];
          sp := 0.0;
          for i := 1 to n do
            sp := sp+z[i,k]*A[i,kw];
          temp := Pythag(sp,zdota[kp]);
          alpha := zdota[kp]/temp;
          beta := sp/temp;
          zdota[kp] := alpha*zdota[k];
          zdota[k] := temp;
          for i := 1 to n do
          begin
            temp := alpha*z[i,kp]+beta*z[i,k];
            z[i,kp] := alpha*z[i,k]-beta*z[i,kp];
            z[i,k] := temp;
          end;
          iact[k] := kw;
          vmultc[k] := vmultc[kp];
          k := kp;
        until k >= nact;
        iact[k] := isave;
        vmultc[k] := vsave;
      end;
      temp := 0.0;
      for i := 1 to n do
        temp := temp+z[i,nact]*A[i,kk];
      if temp = 0.0 then
        goto 490;
      zdota[nact] := temp;
      vmultc[icon] := 0.0;
      vmultc[nact] := ratio;
//
//     Update IACT and ensure that the objective function continues to be
//     treated as the last active constraint when MCON > M.
//
  210:iact[icon] := iact[nact];
      iact[nact] := kk;
      if (mcon > m) and (kk <> mcon) then
      begin
        k := nact-1;
        sp := 0.0;
        for i := 1 to n do
          sp := sp+z[i,k]*A[i,kk];
        temp := Pythag(sp,zdota[nact]);
        alpha := zdota[nact]/temp;
        beta := sp/temp;
        zdota[nact] := alpha*zdota[k];
        zdota[k] := temp;
        for i := 1 to n do
        begin
          temp := alpha*z[i,nact]+beta*z[i,k];
          z[i,nact] := alpha*z[i,k]-beta*z[i,nact];
          z[i,k] := temp;
        end;
        iact[nact] := iact[k];
        iact[k] := kk;
        temp := vmultc[k];
        vmultc[k] := vmultc[nact];
        vmultc[nact] := temp;
      end;
//
//     If stage one is in progress, then set SDIRN to the direction of the next
//     cha>=to the current vector of varia<=.
//
      if mcon > m then goto 320;
      kk := iact[nact];
      temp := 0.0;
      for i := 1 to n do
        temp := temp+sdirn[i]*A[i,kk];
      temp := temp-1.0;
      temp := temp/zdota[nact];

      for i := 1 to n do
        sdirn[i] := sdirn[i]-temp*z[i,nact];
      goto 340;
//
//     Delete the constraint that has the index IACT[ICON] from the active set.
//
260:  if icon < nact then
      begin
        isave := iact[icon];
        vsave := vmultc[icon];
        k := icon;
        repeat
          kp := k+1;
          kk := iact[kp];
          sp := 0.0;
          for i := 1 to n do
            sp := sp+z[i,k]*A[i,kk];
          temp := sqrt(sp*sp+zdota[kp]**2);
          alpha := zdota[kp]/temp;
          beta := sp/temp;
          zdota[kp] := alpha*zdota[k];
          zdota[k] := temp;
          for i := 1 to n do
          begin
            temp := alpha*z[i,kp]+beta*z[i,k];
            z[i,kp] := alpha*z[i,k]-beta*z[i,kp];
            z[i,k] := temp;
          end;
          iact[k] := kk;
          vmultc[k] := vmultc[kp];
          k := kp;
        until k >= nact;
        iact[k] := isave;
        vmultc[k] := vsave;
      end;
      nact := nact-1;

//     If stage one is in progress, then set SDIRN to the direction of the next
//     change to the current vector of variable.
//
      if mcon > m then goto 320;
      temp := 0.0;
      for i := 1 to n do
        temp := temp+sdirn[i]*z[i,nact+1];
      for i := 1 to n do
        sdirn[i] := sdirn[i]-temp*z[i,nact+1];
      goto 340;
//
//     Pick the next search direction of stage two.
//
  320: temp := 1.0/zdota[nact];
      for i := 1 to n do
        sdirn[i] := temp*z[i,nact];
//
//     Calculate the step to the boundary of the trust region or take the step
//     that reduces RESMAX to zero. The two statements below that include the
//     factor 1.0E-6 prevent some harrmless underflows that occurred in a test
//     calculation. Further, we skip the step if it could be zero within a
//     reasonable tolerance for computer rounding errors.
//
  340:dd := rho*rho;
      sd := 0.0;
      ss := 0.0;
      for i := 1 to n do
      begin
        if abs(dx[i]) >= 1.0e-6*rho then
           dd := dd-dx[i]**2;
        sd := sd+dx[i]*sdirn[i];
        ss := ss+sdirn[i]**2;
      end;
      if dd <= 0.0 then goto 490;
      temp := sqrt(ss*dd);
      if abs(sd) >= 1.0e-6*temp then
         temp := sqrt(ss*dd+sd*sd);
      stpful := dd/(temp+sd);
      step := stpful;
      if mcon = m then // first stage, constrain violations
      begin
        acca := step+0.1*resmax;  // actually, test for positivity of
        accb := step+0.2*resmax;  // ResMax
        if (step >= acca) or (acca >= accb) then goto 480;
        step := Min(step,resmax);
      end;
//
//     Set DXNEW to the new variables if STEP is the steplength, and reduce
//     RESMAX to the corresponding maximum residual if stage one is being done.
//     Because DXNEW will be changed during the calculation of some Lagrange
//     multipliers, it will be restored to the following value later.
//
      for i := 1 to n do
        dxnew[i] := dx[i]+step*sdirn[i];
      if (mcon = m) then
      begin
        resold := resmax;
        resmax := 0.0;
        for k := 1 to nact do
        begin
          kk := iact[k];
          temp := b[kk];
          for i := 1 to n do
            temp := temp-A[i,kk]*dxnew[i];
          resmax := Max(resmax,temp);
        end;
      end;
//
//     Set VMULTD to the vmultc vector that would occur if DX became DXNEW. A
//     device is included to force VMULTD[K] = 0.0 if deviations from this value
//     can be attributed to computer rounding errors. First calculate the new
//     Lagrange multipliers.
//
      k := nact;
  390: zdotw := 0.0;
      zdwabs := 0.0;
      for i := 1 to n do
      begin
        temp := z[i,k]*dxnew[i];
        zdotw := zdotw+temp;
        zdwabs := zdwabs+abs(temp);
      end;
      acca := zdwabs+0.1*abs(zdotw);
      accb := zdwabs+0.2*abs(zdotw);
      if (zdwabs >= acca) or (acca >= accb) then
        zdotw := 0.0;
      vmultd[k] := zdotw/zdota[k];
      if k >= 2 then
      begin
          kk := iact[k];
          for i := 1 to n do
            dxnew[i] := dxnew[i]-vmultd[k]*A[i,kk];
          k := k-1;
          goto 390;
      end;
      if mcon > m then
         vmultd[nact] := Max(0.0,vmultd[nact]);
//
//     Complete vmultc by finding the new constraint residuals.
//
      VecAdd(dx[1..N], dxnew[1..N], VecFloatMul(sdirn, step, 1, N, dxnew)[1..N]);
      if mcon > nact then
      begin
          kl := nact+1;
        for k := kl to mcon do
        begin
          kk := iact[k];
          sum := resmax-b[kk];
          sumabs := resmax+abs(b[kk]);
          for i := 1 to n do
          begin
            temp := A[i,kk]*dxnew[i];
            sum := sum+temp;
            sumabs := sumabs+abs(temp);
          end;
          acca := sumabs+0.1*abs(sum);
          accb := sumabs+0.2*abs(sum);
          if (sumabs >= acca) or (acca >= accb) then
            sum := 0.0;
          vmultd[k] := sum;
        end;
      end;
//
//     Calculate the fraction of the step from DX to DXNEW that will be taken.
//
      ratio := 1.0;
      icon := 0;
      for k := 1 to mcon do
      if vmultd[k] < 0.0 then
      begin
        temp := vmultc[k]/(vmultc[k]-vmultd[k]);
        if temp < ratio then
        begin
          ratio := temp;
          icon := k;
        end;
      end;
//
//     Update DX, vmultc and RESMAX.
//
      temp := 1.0-ratio;
      for i := 1 to n do
        dx[i] := temp*dx[i]+ratio*dxnew[i];
      for k := 1 to mcon do
        vmultc[k] := Max(0.0,temp*vmultc[k]+ratio*vmultd[k]);
      if mcon = m then
        resmax := resold+ratio*(resmax-resold);
//
//     If the full step is not acceptable then begin another iteration.
//     Otherwise switch to stage two or end the calculation.
//
      if icon > 0 then goto 70;
      if step = stpful then
      begin
        FinTrsTlp;
        exit;
      end;
  480: mcon := m+1;
      icon := mcon;
      iact[mcon] := mcon;
      vmultc[mcon] := 0.0;
      goto 60;
//
//     We employ any freedom that may be available to reduce the objective
//     function before returning a DX whose length is less than RHO.
//
  490: if mcon = m then goto 480;
       IFull := 0;
       FinTrsTlp;
end;

end.
