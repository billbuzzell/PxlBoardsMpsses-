unit sccpasnet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sccpas;
const
  PakSize = 512;
  (*T0 = 300; T1 = 1000; (*timeouts*)*)
  T0 = 1000; T1 = 3000; (*timeouts*)
  ACK = 10H; NAK = 25H; NPR = 26H; (*acknowledgements*)
  NRQ = 34H; NRS = 35H; (*name request, response*)
  SND = 41H; REC = 42H; MSG = 44H;
  TRQ = 46H; TIM = 47H; (*time requests*)

type
  char8Array = ARRAY [8] of CHAR;
  char8Array_ptr = ^char8Array;
  charArray  = ARRAY of CHAR;
  charArray_ptr = ^charArray;
  byteArray  = ARRAY of BYTE;
  byteArray_ptr = ^byteArray;
  TCharFile  = file of char;
  TCharFile_ptr = ^TCharFile;

PROCEDURE SetPartner(name: char8Array);
PROCEDURE Send(t: BYTE; L: INTEGER; data: byteArray);
PROCEDURE ReceiveHead(timeout: LONGINT);
PROCEDURE FindPartner(name: char8Array; VAR res: INTEGER);
PROCEDURE AppendW(s: LONGINT; VAR d: byteArray; n: INTEGER; VAR k: INTEGER);
PROCEDURE PickS(VAR s: charArray);
PROCEDURE PickQ(VAR w: LONGINT);
PROCEDURE SendData(F: TCharFile);
PROCEDURE ReceiveData(F: TCharFile; VAR done: BOOLEAN);
PROCEDURE reply(msg: INTEGER);
PROCEDURE Serve;
PROCEDURE GetPar1({VAR S: Texts.Scanner});
PROCEDURE GetPar({VAR S: Texts.Scanner; VAR end: LONGINT});

PROCEDURE SendFiles;
PROCEDURE ReceiveFiles;
PROCEDURE SendMsg;
PROCEDURE GetTime;
PROCEDURE StartServer;
PROCEDURE Unprotect;
PROCEDURE WProtect;
PROCEDURE Reset;
PROCEDURE StopServer;
PROCEDURE SCCStatus;

var
   head0, head1: Header;
   partner, dmy: charArray8;
   wprotected: BOOLEAN; { CHANGED from 'protected' to 'wprotected!!!(*write-protection*)}

implementation

PROCEDURE SetPartner(name: char8Array);
BEGIN
  head0.dadr := head1.sadr;
  partner := name}
END;

PROCEDURE Send(t: BYTE; L: INTEGER; data: byteArray);
BEGIN
  head0.typ := t;
  head0.len := L;
  sccpas.SendPacket(head0, data)
END;

PROCEDURE ReceiveHead(timeout: LONGINT);
  VAR time: LONGINT;
BEGIN time := Now() + timeout;
  REPEAT
    sccpas.ReceiveHead(head1);
    IF head1.valid & (head1.sadr # head0.dadr) THEN
      BEGIN
      sccpas.Skip(head1.len);
      head1.valid := FALSE
      END;
    IF head1.valid= false and (Now() >= time) THEN
      BEGIN
      head1.typ := 0FFH END
  UNTIL head1.valid OR (head1.typ = 0FFH)
END;

PROCEDURE FindPartner(name: charArray; VAR res: INTEGER);
VAR
    time: LONGINT;
    k: INTEGER;
    Id: char8Array;

BEGIN
  sccpas.Skip(sccpas.Available());
  res := 0;
  k := 0;
 { WHILE (k < 7) & (name[k] # 0X) DO Id[k] := name[k]; INC(k) END;
  Id[k] := 0X;
  IF Id # partner THEN
    head0.dadr := 0FFH; Send(NRQ, k+1, name); time := Oberon.Time() + T1;
    REPEAT
      sccpas.ReceiveHead(head1);
      IF head1.valid THEN
        IF head1.typ = NRS THEN SetPartner(Id)
        ELSE sccpas.Skip(head1.len); head1.valid := FALSE
        END
      ELSIF Oberon.Time() >= time THEN res := 1; partner[0] := 0X
      END
    UNTIL head1.valid OR (res # 0)
  END  }
END;

PROCEDURE AppendS(s: charArray; VAR d: byteArray; VAR k: INTEGER);
  VAR i: INTEGER; ch: CHAR;
BEGIN
  i := 0;
  REPEAT ch := s[i]; d[k] := ORD(ch);
    INC(i); INC(k) UNTIL ch = 0X
END;

PROCEDURE AppendW(s: LONGINT; VAR d: byteArray; n: INTEGER; VAR k: INTEGER);
  VAR i: INTEGER;
BEGIN {i := 0;
  REPEAT d[k] := s MOD 100H; s := s DIV 100H; INC(i); INC(k) UNTIL i = n}
END;

PROCEDURE PickS(VAR s: charArray);
  VAR i: INTEGER; x: BYTE;
BEGIN{ i := 0;
  REPEAT sccpas.Receive(x); s[i] := CHR(x); INC(i) UNTIL x = 0}
END;

PROCEDURE PickQ(VAR w: LONGINT);
  VAR x0, x1, x2, x3: BYTE;
BEGIN {sccpas.Receive(x0); sccpas.Receive(x1); sccpas.Receive(x2); sccpas.Receive(x3);
  w := x0 + 100H * (x1 + 100H * (x2 + 100H * x3)) }
END;

PROCEDURE SendData(F: TCharFile);
  VAR k, seqno: INTEGER;
    x: BYTE;
    len: LONGINT;
    R: Files.Rider;
    buf: ARRAY PakSize OF BYTE;
BEGIN{ Files.Set(R, F, 0); len := 0; seqno := 0;
  REPEAT k := 0;
    REPEAT Files.ReadByte(R, x);
      IF ~R.eof THEN buf[k] := x; INC(k) END
    UNTIL R.eof OR (k = PakSize);
    REPEAT Send(seqno, k, buf); ReceiveHead(T1)
    UNTIL head1.typ # seqno + ACK;
    seqno := (seqno + 1) MOD 8; len := len + k;
    IF head1.typ # seqno + ACK THEN
      Texts.WriteString(W, " failed"); k := 0
    END
  UNTIL k < PakSize;
  Texts.WriteInt(W, len, 7)}
END;

PROCEDURE ReceiveData(F: TCharFile; VAR done: BOOLEAN);
  VAR k, retry, seqno: INTEGER;
   x: BYTE;
   len: LONGINT;
   R: Files.Rider;
BEGIN {Files.Set(R, F, 0); seqno := 0; len := 0; retry := 2; k := PakSize;
  REPEAT
   IF head1.typ = seqno THEN
     seqno := (seqno + 1) MOD 8; len := len + head1.len; retry := 2;
     Send(seqno + ACK, 0, dmy); k := 0;
     WHILE k < head1.len DO
       sccpas.Receive(x); Files.WriteByte(R, x); INC(k)
     END ;
     IF k < PakSize THEN done := TRUE END
   ELSE DEC(retry);
     IF retry = 0 THEN
       Texts.WriteString(W, " failed"); done := FALSE; k := 0
     END ;
     Send(seqno + ACK, 0, dmy)
   END ;
   ReceiveHead(T0)
  UNTIL k < PakSize;
  Texts.WriteInt(W, len, 7) }
END;

PROCEDURE reply(msg: INTEGER);
BEGIN{
  IF msg = 1 THEN Texts.WriteString(W, " no link")
  ELSIF msg = 2 THEN Texts.WriteString(W, " no permission")
  ELSIF msg = 3 THEN Texts.WriteString(W, " not done")
  ELSIF msg = 4 THEN Texts.WriteString(W, " not found")
  ELSIF msg = 5 THEN Texts.WriteString(W, " no response")
  ELSIF msg = 6 THEN Texts.WriteString(W, " time set")
  END ;
  Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)  }
END;

PROCEDURE Serve;
  VAR i: INTEGER;
    done: BOOLEAN; x: BYTE;
    F: Files.File;
    pw, clock, newclock: LONGINT;
    Id: ARRAY 8 OF CHAR;
    FileName: ARRAY 32 OF CHAR;
BEGIN {
  sccpas.ReceiveHead(head1);
  IF head1.valid THEN
    IF head1.typ = SND THEN
      PickS(Id); PickQ(pw); PickS(FileName);
      Texts.WriteString(W, Id); Texts.Write(W, " ");
Texts.WriteString(W, FileName);
      F := Files.Old(FileName);
      IF F # NIL THEN
        Texts.WriteString(W, " sending"); SetPartner(Id);
        Texts.Append(Oberon.Log, W.buf); SendData(F)
      ELSE Send(NAK, 0, dmy); Texts.Write(W, "~")
      END ;
      reply(0)
    ELSIF head1.typ = REC THEN
      PickS(Id); PickQ(pw); PickS(FileName);
      IF ~protected THEN
        Texts.WriteString(W, Id); Texts.Write(W, " ");
Texts.WriteString(W, FileName);
        F := Files.New(FileName);
        IF F # NIL THEN
          Texts.WriteString(W, " receiving"); SetPartner(Id);
          Texts.Append(Oberon.Log, W.buf);
          Send(ACK, 0, dmy); ReceiveHead(T0); ReceiveData(F, done);
          IF done THEN Files.Register(F) END
        ELSE Send(NAK, 0, dmy); Texts.Write(W, "~")
        END ;
        reply(0)
      ELSE Send(NPR, 0, dmy)
      END
    ELSIF head1.typ = MSG THEN i := 0;
      WHILE i < head1.len DO sccpas.Receive(x); Texts.Write(W, CHR(x));
INC(i) END ;
      Send(ACK, 0, dmy); reply(0)
    ELSIF head1.typ = TRQ THEN
      i := 0; AppendW(Oberon.Clock(), Id, 4, i); Send(TIM, 4, Id)
    ELSIF head1.typ = TIM THEN PickQ(newclock); PickS(Id); PickQ(pw);
      clock := Oberon.Clock();
      IF ~protected & (Id[0] # 0X) & (ABS(pw - clock) > 10) THEN
        Oberon.SetClock(newclock);
        Texts.WriteString(W, Id); Texts.WriteString(W, " changed
System.Date");
        Texts.WriteClock(W, newclock); reply(0)
       END
    ELSIF head1.typ = NRQ THEN
      i := 0;
      REPEAT sccpas.Receive(x); Id[i] := CHR(x); INC(i);
        IF i = 7 THEN Id[7] := 0X; x := 0 END
      UNTIL x = 0;
      WHILE i < head1.len DO sccpas.Receive(x); INC(i) END ;
      IF Id = Oberon.User THEN
        SetPartner(Id); Send(NRS, 0, dmy)
      END
    ELSE sccpas.Skip(head1.len)
    END
  END}
END;

PROCEDURE GetPar1({VAR S: Texts.Scanner});
BEGIN {Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S)}
END;

PROCEDURE GetPar({VAR S: Texts.Scanner; VAR end: LONGINT});
  VAR T: Texts.Text; beg, tm: LONGINT;
BEGIN {Texts.Scan(S);
  IF (S.class = Texts.Char) & (S.c = "^") THEN
    Oberon.GetSelection(T, beg, end, tm);
    IF tm >= 0 THEN Texts.OpenScanner(S, T, beg); Texts.Scan(S) END
  ELSE end := Oberon.Par.text.len
  END  }
END;

PROCEDURE SendFiles;
  VAR k: INTEGER;
    end: LONGINT;
    S: Texts.Scanner;
    F: Files.File;
    buf: ARRAY 64 OF BYTE;
BEGIN{ GetPar1(S);
  IF S.class = Texts.Name THEN
    FindPartner(S.s, k);
    IF k = 0 THEN
      GetPar(S, end);
      WHILE (Texts.Pos(S) < end) & (S.class = Texts.Name) DO
        Texts.WriteString(W, S.s); k := 0;
        AppendS(Oberon.User, buf, k); AppendW(Oberon.Password, buf, 4, k);
        AppendS(S.s, buf, k);
        IF S.nextCh = ":" THEN (*prefix*)
          Texts.Scan(S); Texts.Scan(S);
          IF S.class = Texts.Name THEN
            buf[k-1] := ORD("."); AppendS(S.s, buf, k);
            Texts.Write(W, ":"); Texts.WriteString(W, S.s)
          END
        END ;
        F := Files.Old(S.s);
        IF F # NIL THEN
          Send(REC, k, buf); ReceiveHead(T0);
          IF head1.typ = ACK THEN
            Texts.WriteString(W, " sending"); Texts.Append(Oberon.Log,
W.buf);
            SendData(F); reply(0)
          ELSIF head1.typ = NPR THEN reply(2); end := 0
          ELSIF head1.typ = NAK THEN reply(3); end := 0
          ELSE reply(5); end := 0
          END
        ELSE reply(4)
        END ;
        Texts.Scan(S)
      END
    ELSE reply(1)
    END
  END }
END;

PROCEDURE ReceiveFiles;
  VAR k: INTEGER; done: BOOLEAN;
    end: LONGINT;
    S: Texts.Scanner;
    F: Files.File;
    buf: ARRAY 64 OF BYTE;
BEGIN{ GetPar1(S);
  IF S.class = Texts.Name THEN
    FindPartner(S.s, k);
    IF k = 0 THEN
      GetPar(S, end);
      WHILE (Texts.Pos(S) < end) & (S.class = Texts.Name) DO
        Texts.WriteString(W, S.s);
        k := 0; AppendS(Oberon.User, buf, k); AppendW(Oberon.Password,
buf, 4, k);
        AppendS(S.s, buf, k);
        IF S.nextCh = ":" THEN (*prefix*)
          Texts.Scan(S); Texts.Scan(S);
          IF S.class = Texts.Name THEN
            buf[k-1] := ORD("."); AppendS(S.s, buf, k);
            Texts.Write(W, ":"); Texts.WriteString(W, S.s)
          END
        END ;
        Send(SND, k, buf);
        Texts.WriteString(W, " receiving"); Texts.Append(Oberon.Log,
W.buf);
        ReceiveHead(T1);
        IF head1.typ = 0 THEN
          F := Files.New(S.s);
          IF F # NIL THEN
            ReceiveData(F, done);
            IF done THEN Files.Register(F); reply(0) ELSE end := 0 END
          ELSE reply(3); Send(NAK, 0, dmy)
          END
        ELSIF head1.typ = NAK THEN reply(4)
        ELSIF head1.typ = NPR THEN reply(2); end := 0
        ELSE reply(5); end := 0
        END ;
        Texts.Scan(S)
      END
    ELSE reply(1)
    END
  END }
END;

PROCEDURE SendMsg;
  VAR i: INTEGER; ch: CHAR;
    S: Texts.Scanner;
    msg: ARRAY 64 OF BYTE;
BEGIN {GetPar1(S);
  IF S.class = Texts.Name THEN
    FindPartner(S.s, i);
    IF i = 0 THEN
      Texts.Read(S, ch);
      WHILE (ch >= " ") & (i < 64) DO
        msg[i] := ORD(ch); INC(i); Texts.Read(S, ch)
      END ;
      Send(MSG, i, msg); ReceiveHead(T0);
      IF head1.typ # ACK THEN reply(3) END
    ELSE reply(1)
    END
  END}
END;

PROCEDURE GetTime;
  VAR dt, res: INTEGER;
    S: Texts.Scanner;
BEGIN{ GetPar1(S);
  IF S.class = Texts.Name THEN
    FindPartner(S.s, res);
    IF res = 0 THEN
      Send(TRQ, 0, dmy); ReceiveHead(T1);
      IF head1.typ = TIM THEN
        PickQ(dt); Oberon.SetClock(dt); reply(6)
      END
    ELSE reply(1)
    END
  END  }
END;

PROCEDURE StartServer;
BEGIN {wprotected := TRUE; partner[0] := 0X; sccpas.Start(TRUE);
  Oberon.Remove(Server); Oberon.Install(Server);
  Texts.WriteString(W, " Server started as "); Texts.WriteString(W,
Oberon.User);
  Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)}
END;

PROCEDURE Unprotect;
BEGIN {wprotected := FALSE }
END;

PROCEDURE WProtect;
BEGIN{ protected := TRUE }
END;

PROCEDURE Reset;
BEGIN {sccpas.Start(TRUE)  }
END;

PROCEDURE StopServer;
BEGIN {Oberon.Remove(Server); Texts.WriteString(W, " Server stopped");
  Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf) }
END;

PROCEDURE sccpasStatus;
BEGIN{
  Texts.WriteString(W, "sccpas.Available() ");
  Texts.WriteInt(W, sccpas.Available(), 1);
  Texts.WriteLn(W);
  Texts.Append(Oberon.Log, W.buf) }
END;

{BEGIN Texts.OpenWriter(W); Server := Oberon.NewTask(Serve, 500)
END Net.}

end.

