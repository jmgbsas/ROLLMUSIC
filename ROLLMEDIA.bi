#Include "window9.bi"


Dim  As Integer event,mov8,MOV_FLAG
Dim  As HWND  hwnd


hwnd=OpenWindow("MEDIA",10,10,350,350) : CenterWindow(hwnd)


ButtonGadget(1,10,280,20,20,"X"):GadgetToolTip(1,"STOP")
ButtonGadget(2,40,280,20,20,"|>"):GadgetToolTip(2,"PLAY")
ButtonGadget(3,70,280,20,20,"||"):GadgetToolTip(3,"PAUSE")
ButtonGadget(6,100,280,20,20,"<<"):GadgetToolTip(6,"Playback speed")
ButtonGadget(7,130,280,20,20,">>"):GadgetToolTip(7,"Increase the playback speed")
ButtonGadget(8,160,280,20,20,"+"):GadgetToolTip(8,"Open File")
ImageGadget(4,6,0,320,240)
TrackBarGadget(5,5,250,320,20,0,10,TBS_NOTICKS  )




SetTimer(hwnd,1,10,Cast(TIMERPROC,@resizeMEDIA()))


Do
   event=WaitEvent()
   If Event=EventClose Then Exit Do
   If event=EventGadget Then
      Select case EventNumber
         Case 1 : If mov8 Then stopmovie(mov8)
         Case 2 : If mov8 Then Playmovie(mov8):SetRateMovie(mov8,1)
         Case 3 : If mov8 Then Pausemovie(mov8)
         case 5
            If GetAsyncKeyState(1)<0 Then
               if mov8 Then
                  MovieSetPositions(mov8,cast(double,getTrackBarPos(5))*1000000,GetEndPosMovie(mov8) )
               EndIf
            EndIf
         Case 6 : If mov8 Then SetRateMovie(mov8,GetRateMovie(mov8)-0.01)
         Case 7 : If mov8 Then SetRateMovie(mov8,GetRateMovie(mov8)+0.01)
         Case 8
            #ifdef UNICODE
               Var OFR = OpenFileRequester("","C:\","Media files (*.avi, *.mp3, *.wmv, *.wav, *.mp4, *.mp2, *.mp1)|*.avi; *.mp3; *.wmv; *.wav; *.mp4; *.mp2; *.mp1|")          
            #else 
               Var OFR = OpenFileRequester("","C:\","Media files (*.avi, *.mp3, *.wmv, *.wav, *.mp4, *.mp2, *.mp1)"+Chr(0)+"*.avi; *.mp3; *.wmv; *.wav; *.mp4; *.mp2; *.mp1"+Chr(0))           
            #EndIf
            If OFR<>"" Then
               If mov8 Then
                  FreeMovie(mov8)
               EndIf
               mov8=loadmovie(GadgetID(4),OFR,0,0,WindowWidth(hwnd)-30,WindowHeight(hwnd)-110)
               SetTrackBarMaxPos(5,Int(GetEndPosMovie(mov8)/1000000 ))
               Playmovie(mov8):SetRateMovie(mov8,1)
               MOV_FLAG=1
            EndIf
      End Select
   EndIf
Loop


if mov8 then FreeMovie(mov8)
