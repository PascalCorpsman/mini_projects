Unit urgb_physics;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uvectormath;

Const
  Gravity: TVector2 = (x: 0; y: 9.8);

Type
  TContactReason = (
    crDie // Die Karte wurde verlassen
    , crWin
    , crRemoveDown
    , crRemoveUp
    , crRemoveLeft
    , crRemoveRight
    , crChangeToR
    , crChangeToG
    , crChangeToB
    );

  TContactReasons = Set Of TContactReason;

Implementation

End.

