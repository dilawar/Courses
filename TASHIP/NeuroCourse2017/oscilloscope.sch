<Qucs Schematic 0.0.19>
<Properties>
  <View=0,0,985,800,1,0,0>
  <Grid=10,10,1>
  <DataSet=oscilloscope.dat>
  <DataDisplay=oscilloscope.dpl>
  <OpenDisplay=1>
  <Script=oscilloscope.m>
  <RunScript=0>
  <showFrame=0>
  <FrameText0=Title>
  <FrameText1=Drawn By:>
  <FrameText2=Date:>
  <FrameText3=Revision:>
</Properties>
<Symbol>
</Symbol>
<Components>
  <GND * 1 480 420 0 0 0 0>
  <GND * 1 770 410 0 0 0 0>
  <VProbe Pr1 1 910 340 -16 28 0 3>
  <Vdc V1 1 550 390 -94 -26 0 3 "0.060 V" 1>
  <C C1 1 480 360 17 -26 0 1 "1 pF" 1 "" 0 "neutral" 0>
  <R R2 1 550 330 15 -26 0 1 "40 MOhm" 1 "26.85" 0 "0.0" 0 "0.0" 0 "26.85" 0 "US" 0>
  <Irect I2 1 410 330 -103 -26 1 1 "4 nA" 1 "0.1 ms" 1 "0.1 ms" 1 "1 ns" 0 "1 ns" 0 "0 ns" 0>
  <.TR TR1 1 150 100 0 77 0 0 "lin" 1 "0" 1 "2 ms" 1 "2001" 0 "Trapezoidal" 0 "2" 0 "1 ns" 0 "1e-16" 0 "150" 0 "0.001" 0 "1 pA" 0 "1 uV" 0 "26.85" 0 "1e-3" 0 "1e-6" 0 "1" 0 "CroutLU" 0 "no" 0 "yes" 0 "0" 0>
  <R R3 1 770 330 15 -26 0 1 "1 MOhm" 1 "26.85" 0 "0.0" 0 "0.0" 0 "26.85" 0 "US" 0>
</Components>
<Wires>
  <770 270 770 300 "" 0 0 0 "">
  <770 300 880 300 "" 0 0 0 "">
  <880 300 880 330 "" 0 0 0 "">
  <880 330 890 330 "" 0 0 0 "">
  <890 350 890 370 "" 0 0 0 "">
  <770 370 770 410 "" 0 0 0 "">
  <770 370 890 370 "" 0 0 0 "">
  <550 270 770 270 "IN" 610 240 30 "">
  <480 270 550 270 "" 0 0 0 "">
  <550 270 550 300 "" 0 0 0 "">
  <480 420 550 420 "" 0 0 0 "">
  <410 420 480 420 "" 0 0 0 "">
  <410 360 410 420 "" 0 0 0 "">
  <410 270 480 270 "" 0 0 0 "">
  <410 270 410 300 "" 0 0 0 "">
  <480 270 480 330 "" 0 0 0 "">
  <480 390 480 420 "" 0 0 0 "">
  <770 360 770 370 "" 0 0 0 "">
</Wires>
<Diagrams>
</Diagrams>
<Paintings>
</Paintings>
