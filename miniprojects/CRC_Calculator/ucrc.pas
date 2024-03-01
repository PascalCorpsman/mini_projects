(******************************************************************************)
(* ucrc.pas                                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : FPC-Module for calculation CRC-Checksums                     *)
(*     Original .c File taken from here :                                     *)
(*                        http://www.zorc.breitbandkatze.de/crc.html          *)
(*   Original Source for .c File taken from here : http://www.tty1.net/pycrc/ *)
(*   and translated by Corpsman www.Corpsman.de to Freepascal                 *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Added CalcCRC32                                       *)
(*               0.03 - Added CalculateCRClen                                 *)
(*               0.04 - Fixed CRC-Calculation for Order = 32                  *)
(*                                                                            *)
(******************************************************************************)
Unit ucrc;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

{$IFDEF UNIX}
  TBytes = Array Of Byte;
{$ENDIF}

  TDirectMode = (dmDirect, dmNondirect);

  { TCRC_calculator }

  TCRC_Calculator = Class
  private
    fTableInitialized: Boolean;
    fcrctab: Array[0..255] Of Int32;
    FCRCMask: int32;
    FCRCHighBit: int32;
    fGeneratorPolynom: int32;
    Forder: integer;
    fcrcinit,
      fcrcinit_direct: int32;
    fRefIn: Boolean;
    fdirect: TDirectMode;
    Procedure setGeneratorPolynom(value: int32);
    Procedure SetOrder(Value: integer);
    Function reflect(crc: int32; bitnum: integer): int32;
    Function calc_by_table(Const Data: TBytes; Bytecount: integer): Int32;
    Function calc_by_bit(Const Data: TBytes; Bytecount: integer): Int32;
    Procedure setStartValue(value: int32);
    Procedure setRefIn(Value: Boolean);
    Procedure SetDirect(value: TDirectMode);
    (*
     * Lookup Tabellen werden erzeugt aus :
     * fRefin
     * fOrder (indirect wegen FHighBit, FCRCMask)
     * FGeneratorPolynom
     *)
    Procedure GenerateCRCTable(); // Ist die Order durch 8 Teilbar, dann kann mit Lookup Tabellen gearbeitet werden
    Function CheckSettings(): Boolean;
  public
    RefOut: Boolean; // specifies if the CRC will be reflected before XOR
    CRCXor: Int32; // is the final Xor value
    Property Direct: TDirectMode read fDirect write SetDirect; // specifies the kind of algorithm: 1=direct, no augmented zero bits
    Property RefIn: Boolean read FRefin write setRefIn; // specifies if a data byte is reflected before processing (UART) or not
    Property crcInit: int32 read fcrcinit write setStartValue; // is the initial CRC value belonging to that algorithm
    Property Polynom: int32 read fGeneratorPolynom write setGeneratorPolynom;
    Property Order: integer read Forder write setOrder;
    Function ConvertCRCdirect(CRC_init: int32; From_direct, To_direct: TDirectMode): int32;
    Constructor create();
    Destructor Destroy; override;
    Function CalculateCRC(Const Data: TBytes): Int32;
    Function CalculateCRClen(Const Data: TBytes; Len: integer): Int32; // Berechnet das CRC nur über die ersten Len Bytes
  End;

  (* Quick and dirty 32-Bit CRC- Calulation, could also be done by TCRC_Calculator *)
Procedure CalcCRC32(Data: TStream; Var CRCValue: DWORD);

Implementation

// The constants here are for the CRC-32 generator
// polynomial, as defined in the Microsoft
// Systems Journal, March 1995, pp. 107-108

Const
  table: Array[0..255] Of DWORD = (
    $00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

  //Given the above  lookup table, the code for computing a CRC-32 is as follows (see initialization/finalization below):
  // Use CalcCRC32 as a procedure so CRCValue can be passed in but
  // also returned. This allows multiple calls to CalcCRC32 for
  // the "same" CRC-32 calculation.

Procedure CalcCRC32(Data: TStream; Var CRCValue: DWORD);
// The following is a little cryptic (but executes very quickly).
// The algorithm is as follows:
// 1. exclusive-or the input byte with the low-order byte of
// the CRC register to get an INDEX
// 2. shift the CRC register eight bits to the right
// 3. exclusive-or the CRC register with the contents of Table[INDEX]
// 4. repeat steps 1 through 3 for all bytes

Var
  i: DWORD;
  q: BYTE;
  ByteCount: Int64;
Begin
  ByteCount := data.Size - data.Position;
  If ByteCount = 0 Then exit;
  For i := 0 To ByteCount - 1 Do Begin
    q := 0;
    data.Read(q, 1);
    CRCvalue := (CRCvalue Shr 8) Xor Table[q Xor (CRCvalue And $000000FF)];
  End;
End;

{ TCRC_calculator }

Constructor TCRC_Calculator.create;
Begin
  Inherited create;
  // Initialisieren für Modbus- Kommunikationen
  Forder := 0;
  FGeneratorPolynom := 0;
  setGeneratorPolynom($8005);
  fcrcinit := 0;
  fRefIn := true;
  RefOut := true;
  //setStartValue($FFFF);
  SetOrder(16); // - Das setzt fcrcinit auf $FFFF
  Direct := dmdirect;
  CRCXor := 0;
  fTableInitialized := false;
End;

Destructor TCRC_Calculator.Destroy;
Begin

End;

Procedure TCRC_Calculator.GenerateCRCTable;
Var
  i, j: integer;
  crc: int32;
  bit: int32;
Begin
  // Es muss neu Erzeugt werden
  If (Not fTableInitialized) Then Begin
    // make CRC lookup table used by table algorithms
    For i := 0 To 255 Do Begin
      crc := i;
      If (frefin) Then crc := reflect(crc, 8);
      crc := crc Shl (order - 8);
      For j := 0 To 7 Do Begin
        bit := crc And FCRCHighBit;
        crc := crc Shl 1;
        If (bit <> 0) Then Begin
          crc := crc Xor FGeneratorPolynom;
        End;
      End;
      If fRefIn Then crc := reflect(crc, Order);
      crc := crc And FCRCMask;
      fcrctab[i] := crc;
    End;
    fTableInitialized := true;
  End;
End;

Function TCRC_Calculator.CheckSettings: Boolean;
Begin
  result := false;
  // 1. Plausibilitätscheck
  If ((fGeneratorPolynom And FCRCMask) <> fGeneratorPolynom) Then Begin
    Raise exception.Create(format('Error invalid generator polynom (%d) for order (%d).', [fGeneratorPolynom, Forder]));
  End;
  If ((CRCXor And FCRCMask) <> CRCXor) Then Begin
    Raise exception.Create(format('Error invalid final XOR value (%d) for order (%d).', [CRCXor, Forder]));
  End;
  If ((fcrcinit And FCRCMask) <> fcrcinit) Then Begin
    Raise exception.Create(format('Error invalid CRC init value (%d) for order (%d).', [fcrcinit, Forder]));
  End;
  result := true;
End;

Function TCRC_Calculator.CalculateCRC(Const Data: TBytes): Int32;
Begin
  If Not CheckSettings() Then exit;
  fcrcinit_direct := ConvertCRCdirect(crcInit, Direct, dmDirect);
  // Berechnen des CRC's
  If (Forder Mod 8 = 0) Then Begin // Wenn Beschleunigt via Lookup Table gearbeitet werden kann.
    result := calc_by_table(data, length(data));
  End
  Else Begin // Wenn Bitweise Gerechnet werden muss, weil die Ordnung des Generatorpolynom nicht durch 8 Teilbar ist.
    result := calc_by_bit(data, length(data));
  End;
End;

Function TCRC_Calculator.CalculateCRClen(Const Data: TBytes; Len: integer
  ): Int32;
Begin
  If len > length(data) Then
    Raise exception.Create(format('Error invalid len %d', [len]));
  If Not CheckSettings() Then exit;
  fcrcinit_direct := ConvertCRCdirect(crcInit, Direct, dmDirect);
  // Berechnen des CRC's
  If (Forder Mod 8 = 0) Then Begin // Wenn Beschleunigt via Lookup Table gearbeitet werden kann.
    result := calc_by_table(data, len);
  End
  Else Begin // Wenn Bitweise Gerechnet werden muss, weil die Ordnung des Generatorpolynom nicht durch 8 Teilbar ist.
    result := calc_by_bit(data, len);
  End;
End;

Procedure TCRC_Calculator.setGeneratorPolynom(value: int32);
Begin
  If value <> fGeneratorPolynom Then Begin
    fGeneratorPolynom := value;
    fTableInitialized := false;
  End;
End;

Procedure TCRC_Calculator.SetOrder(Value: integer);
Begin
  If (Value <> Forder) And (value > 0) And (value <= 32) Then Begin
    Forder := Value;
    FCRCMask := (((1 Shl (Forder - 1)) - 1) Shl 1) Or 1;
    FCRCHighBit := (1 Shl (Forder - 1));
    crcInit := FCRCMask;
    fTableInitialized := false;
  End;
End;

Function TCRC_Calculator.reflect(crc: int32; bitnum: integer): int32;
Var
  i, j, crcout: int32;
Begin
  // reflects the lower 'bitnum' bits of 'crc'
  j := 1;
  crcout := 0;
  i := 1 Shl (bitnum - 1);
  While bitnum >= 1 Do Begin
    If (crc And i) <> 0 Then Begin
      crcout := crcout Or j;
    End;
    i := i Shr 1;
    j := j Shl 1;
    dec(bitnum);
  End;
  result := crcout;
End;

Function TCRC_Calculator.calc_by_table(Const Data: TBytes; Bytecount: integer): Int32;
Var
  crc: Int32;
  i: integer;
Begin
  If Not fTableInitialized Then Begin
    GenerateCRCTable();
  End;
  // fast lookup table algorithm without augmented zero bytes, e.g. used in pkzip.
  // only usable with polynom orders of 8, 16, 24 or 32.
  crc := fcrcinit_direct;
  If (frefin) Then crc := reflect(crc, order);
  If (Not frefin) Then Begin
    For i := 0 To Bytecount - 1 Do Begin
      crc := (crc Shl 8) Xor fcrctab[((crc Shr (order - 8)) And $FF) Xor data[i]];
    End;
  End
  Else Begin
    For i := 0 To Bytecount - 1 Do Begin
      crc := (crc Shr 8) Xor fcrctab[(crc And $FF) Xor data[i]];
    End;
  End;
  If (refout Xor frefin) Then crc := reflect(crc, order);
  crc := crc Xor crcxor;
  crc := crc And FCRCMask;
  result := crc;
End;

Function TCRC_Calculator.calc_by_bit(Const Data: TBytes; Bytecount: integer): Int32;
Var
  bit, c, j: int32;

  i: integer;
  crc: int32;
Begin
  // fast bit by bit algorithm without augmented zero bytes.
  // does not use lookup table, suited for polynom orders between 1...32.

//  	unsigned long i, j, c, bit;
  crc := fcrcinit_direct;

  //for (i=0; i<len; i++) {
  For i := 0 To Bytecount - 1 Do Begin

    c := data[i];
    If (refin) Then Begin
      c := reflect(c, 8);
    End;
    j := $80;
    //  		for (j=0x80; j; j>>=1) {
    While j > 0 Do Begin
      bit := crc And fcrchighbit;
      crc := crc Shl 1;
      If ((c And j) <> 0) Then Begin
        bit := bit Xor fcrchighbit;
      End;
      If (bit <> 0) Then Begin
        crc := crc Xor polynom;
      End;
      j := j Shr 1;
    End;
  End;

  If (refout) Then Begin
    crc := reflect(crc, order);
  End;
  crc := crc Xor crcxor;
  crc := crc And fcrcmask;

  result := crc;
End;

Procedure TCRC_Calculator.setStartValue(value: int32);
Begin
  // Alles was in den Create Tables Gebraucht wird, muss bei dessen Änderung die Tabellen verwerfen
  If value <> fcrcinit Then Begin
    fcrcinit := value;
  End;
End;

Procedure TCRC_Calculator.setRefIn(Value: Boolean);
Begin
  // Alles was in den Create Tables Gebraucht wird, muss bei dessen Änderung die Tabellen verwerfen
  If value <> fRefIn Then Begin
    fRefIn := value;
    fTableInitialized := false;
  End;
End;

Procedure TCRC_Calculator.SetDirect(value: TDirectMode);
Begin
  // Alles was in den Create Tables Gebraucht wird, muss bei dessen Änderung die Tabellen verwerfen
  If value <> fdirect Then Begin
    fdirect := value;
    fTableInitialized := false;
  End;
End;

Function TCRC_Calculator.ConvertCRCdirect(CRC_init: int32; From_direct,
  To_direct: TDirectMode): int32;
Var
  crc: int32;
  i: Integer;
  bit: Int32;
Begin
  If From_direct = To_direct Then Begin
    // Nichts zu tun
    result := CRC_init;
  End
  Else Begin
    If From_direct = dmDirect Then Begin
      // Umwandlung von Direct in Nondirect
      crc := crc_init;
      //  for (i=0; i<order; i++) {
      For i := 0 To order - 1 Do Begin
        //   bit = crc & 1;
        bit := crc And $1;
        If (bit <> 0) Then Begin
          crc := crc Xor polynom;
        End;
        crc := crc Shr 1;
        If (bit <> 0) Then Begin
          crc := crc Or fcrchighbit;
        End;
      End;
    End
    Else Begin
      // Umwandlung von NonDirect in direct
      crc := crc_init;
      For i := 0 To order - 1 Do Begin
        bit := crc And FCRCHighBit;
        crc := crc Shl 1;
        If (bit <> 0) Then Begin
          crc := crc Xor polynom;
        End;
      End;
      crc := crc And fcrcmask;
    End;
    result := crc;
  End;
End;

End.

