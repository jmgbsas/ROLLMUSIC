'
'' FreeBASIC gfx library constants
'' modificado por jmg 
#ifndef __fbgfx_bi__
#define __fbgfx_bi__

#inclib "fbgfx"
#ifdef __FB_WIN32__
	#inclib "gdi32"
	#inclib "winmm"
	#inclib "user32"
#elseif defined(__FB_LINUX__)
	#libpath "/usr/X11R6/lib"
	#inclib "X11"
	#inclib "Xext"
	#inclib "Xpm"
	#inclib "Xrandr"
	#inclib "Xrender"
	#inclib "pthread"
#endif


# if __FB_LANG__ = "fb"
namespace FB
# endif

	'' Flags accepted by Screen and ScreenRes
	''
	'' Usage examples:
	''	SCREEN 14, 16,, GFX_FULLSCREEN
	''	SCREEN 18, 32,, GFX_OPENGL OR GFX_STENCIL_BUFFER
	''
	const as integer GFX_NULL 					= -1		, _
					 GFX_WINDOWED				= &h00		, _
					 GFX_FULLSCREEN				= &h01		, _
					 GFX_OPENGL					= &h02		, _
					 GFX_NO_SWITCH				= &h04		, _
					 GFX_NO_FRAME				= &h08		, _
					 GFX_SHAPED_WINDOW			= &h10		, _
					 GFX_ALWAYS_ON_TOP			= &h20		, _
					 GFX_ALPHA_PRIMITIVES		= &h40		, _
					 GFX_HIGH_PRIORITY			= &h80      , _
					 GFX_SCREEN_EXIT            = &h80000000 
	'' OpenGL options
	const as integer GFX_STENCIL_BUFFER			= &h10000	, _
					 GFX_ACCUMULATION_BUFFER	= &h20000	, _
					 GFX_MULTISAMPLE			= &h40000

	'Constants for OpenGL 2D render
	const as integer OGL_2D_NONE        		= 0, _
					 OGL_2D_MANUAL_SYNC			= 1, _
                     OGL_2D_AUTO_SYNC			= 2
					 
	'' Constants accepted by ScreenControl
	''
	'' Getters:
	const as integer GET_WINDOW_POS				= 0		, _
					 GET_WINDOW_TITLE			= 1		, _
					 GET_WINDOW_HANDLE			= 2		, _
					 GET_DESKTOP_SIZE			= 3		, _
					 GET_SCREEN_SIZE			= 4		, _
					 GET_SCREEN_DEPTH			= 5		, _
					 GET_SCREEN_BPP				= 6		, _
					 GET_SCREEN_PITCH			= 7		, _
					 GET_SCREEN_REFRESH			= 8		, _
					 GET_DRIVER_NAME			= 9		, _
					 GET_TRANSPARENT_COLOR		= 10	, _
					 GET_VIEWPORT				= 11	, _
					 GET_PEN_POS				= 12	, _
					 GET_COLOR					= 13	, _
					 GET_ALPHA_PRIMITIVES		= 14	, _
					 GET_GL_EXTENSIONS			= 15	, _
					 GET_HIGH_PRIORITY			= 16
	'' Setters:
	const as integer SET_WINDOW_POS				= 100	, _
					 SET_WINDOW_TITLE			= 101	, _
					 SET_PEN_POS				= 102	, _
					 SET_DRIVER_NAME			= 103	, _
					 SET_ALPHA_PRIMITIVES		= 104	, _
					 SET_GL_COLOR_BITS			= 105	, _
					 SET_GL_COLOR_RED_BITS		= 106	, _
					 SET_GL_COLOR_GREEN_BITS	= 107	, _
					 SET_GL_COLOR_BLUE_BITS		= 108	, _
					 SET_GL_COLOR_ALPHA_BITS	= 109	, _
					 SET_GL_DEPTH_BITS			= 110	, _
					 SET_GL_STENCIL_BITS		= 111	, _
					 SET_GL_ACCUM_BITS			= 112	, _
					 SET_GL_ACCUM_RED_BITS		= 113	, _
					 SET_GL_ACCUM_GREEN_BITS	= 114	, _
					 SET_GL_ACCUM_BLUE_BITS		= 115	, _
					 SET_GL_ACCUM_ALPHA_BITS	= 116	, _
					 SET_GL_NUM_SAMPLES			= 117	, _
					 SET_GL_2D_MODE				= 150	, _
					 SET_GL_SCALE				= 151
	'' Commands:
	const as integer POLL_EVENTS				= 200
	

	'' Color values for transparency
	''
	const as integer MASK_COLOR_INDEX			= 0, _
					 MASK_COLOR					= &hFF00FF


	'' Event type IDs
	''
	const as integer EVENT_KEY_PRESS			= 1		, _
					 EVENT_KEY_RELEASE			= 2		, _
					 EVENT_KEY_REPEAT			= 3		, _
					 EVENT_MOUSE_MOVE			= 4		, _
					 EVENT_MOUSE_BUTTON_PRESS	= 5		, _
					 EVENT_MOUSE_BUTTON_RELEASE	= 6		, _
					 EVENT_MOUSE_DOUBLE_CLICK	= 7		, _
					 EVENT_MOUSE_WHEEL			= 8		, _
					 EVENT_MOUSE_ENTER			= 9		, _
					 EVENT_MOUSE_EXIT			= 10	, _
					 EVENT_WINDOW_GOT_FOCUS		= 11	, _
					 EVENT_WINDOW_LOST_FOCUS	= 12	, _
					 EVENT_WINDOW_CLOSE			= 13	, _
					 EVENT_MOUSE_HWHEEL			= 14


	'' Event structure, to be used with ScreenEvent
	''
	type EVENT field = 1
		type as long
		union
			type
				scancode as long
				ascii as long
			end type
			type
				x as long
				y as long
				dx as long
				dy as long
			end type
			button as long
			z as long
			w as long
		end union
	end type


	'' Image buffer header, old style
	''
	type _OLD_HEADER field = 1
		bpp : 3 as ushort
		width : 13 as ushort
		height as ushort
	end type


	'' Image buffer header, new style (incorporates old header)
	''
	type IMAGE field = 1
		union
			old as _OLD_HEADER
			type as ulong
		end union
		bpp as long
		width as ulong
		height as ulong
		pitch as ulong
		_reserved(1 to 12) as ubyte
		
'		'' properties
'		declare property pixels() as ubyte ptr
	end type
	
	'' This is a trick to obtain a pointer to the pixels data area
	''
'	property IMAGE.pixels() as ubyte ptr
'		return cast(ubyte ptr, @this) + sizeof(IMAGE)
'	end property
	
	type PUT_HEADER as IMAGE
	

	'' Constant identifying new style headers
	'' (image.type must be equal to this value in new style headers)
	''
	const as integer PUT_HEADER_NEW				= &h7


	'' Mouse button constants to be used with GETMOUSE/ScreenEvent
	''
	const as integer BUTTON_LEFT				= &h1	, _
					 BUTTON_RIGHT				= &h2	, _
					 BUTTON_MIDDLE				= &h4	, _
					 BUTTON_X1					= &h8	, _
					 BUTTON_X2					= &h10


	'' Keyboard scancodes returned by MULTIKEY
	''
	' JMG Release codes = Press code + 128. Pause/Break may lock code returns.
	enum ' tested and update with spanish keyoard PRESS CODE
        SC_ESCAPE = 1
        SC_1  = 2  
        SC_2  = 3 
        SC_3  = 4
        SC_4  = 5
        SC_5  = 6
        SC_6  = 7
        SC_7  = 8
        SC_8  = 9
        SC_9  = 10
        SC_0  = 11
        SC_MINUS = 12  ' normal and keypad
        SC_PLUS  = 13  ' normal keyboard spanish 
        SC_BACKSPACE   =  14
        SC_TAB  = 15
        SC_Q  = 16
        SC_W  = 17
        SC_E  = 18
        SC_R  = 19
        SC_T  = 20
        SC_Y  = 21
        SC_U  = 22
        SC_I  = 23
        SC_O  = 24
        SC_P  = 25
        SC_LEFTBRACKET = 26 '  TILDE spanish keyboard
        SC_RIGHTBRACKET = 27 ' ¡ spanish keyboard
        SC_ENTER  = 28
        SC_CONTROL = 29 '1D..LEFT, RIGHT = 224, 29 
        SC_A = 30 ' 1E 
        SC_S = 31
        SC_D = 32 ' h20
        SC_F = 33
        SC_G = 34 ' h22
        SC_H = 35 
        SC_J = 36
        SC_K = 37
        SC_L = 38
        SC_SEMICOLON = 39 ' &h27 ; ..+ shift= :
        SC_QUOTE = 40 ' ('..+shift= " )
        SC_TILDE  = 41 ' (`) &h29... ñ in spanish
        SC_LSHIFT = 42  '  &h2A
        SC_BACKSLASH = 43 'spanish keyboard = (º)
        SC_Z   = 44 ' 2C
        SC_X   = 45 ' 2D
        SC_C   = 46
        SC_V   = 47
        SC_B   = 48 ' h30
        SC_N   = 49
        SC_M   = 50
        SC_COMMA = 51 ' 
        SC_PERIOD = 52 ' normal keypad 
        SC_SLASH = 53 ' keypad /, normal spanish keyoard = ç ¿?
        SC_RSHIFT = 54  
        SC_MULTIPLY = 55 ' keypad * H37
        SC_ALT = 56 
        SC_SPACE = 57 ' h39
        SC_CAPSLOCK = 58 ' h3A
        SC_F1 = 59   ' h3B
        SC_F2 = 60   ' h3C 
        SC_F3 = 61   ' h3D
        SC_F4 = 62   ' h3E
        SC_F5 = 63   ' h3F
        SC_F6  = 64  ' &h40
        SC_F7  = 65  ' &h41
        SC_F8  = 66
        SC_F9  = 67
        SC_F10 = 68
        SC_NUMLOCK = 69
        SC_SCROLLLOCK = 70
        SC_HOME = 71
        SC_UP   = 72      
        SC_PAGEUP  = 73  ' &h49
        SC_KEYPADMINUS = 74 '' &h4A unused (?) ' keypad - (12), should be 74
        SC_LEFT = 75 ' E0 4B, 224 75
        SC_CLEAR = 76 ' KEYPAD 5, MULTIKEY NO DETECT? TEST
        SC_RIGHT = 77  
        SC_KEYPADPLUS =  78 ' keypad +
        SC_END  =  79 'normal and keypad
        SC_DOWN  = 80 'normal and keypad
        SC_PAGEDOWN = 81 'normal and keypad
        SC_INSERT = 82 'normal and keypad
        SC_DELETE = 83  'normal or (+numlock=keypad period, -numlock=keypd delete) )
        '' &h54        'shift + F1 should be http://philipstorr.id.au/pcbook/book3/table1.htm
        SC_SHIFTF2 = 85 '' &h55        'shift + F2 should be 
        '' &h56        'shift + F3 should be 
        SC_F11      = 87   ' shift + F4 should be  
        SC_F12      = 88   'shift + F5 should be 
        '' &h59          ' shift + F6 should be 
        '' &h5A          'shift + F7 should be  
        SC_LWIN       = 91 '&h5B 'shift + F8 should be 
        SC_RWIN       = 92
        SC_MENU       = 93
        '' &h5E       ' Ctrl F1 should be
        '' &h5F       ' Ctrl F2 should be
        '' &h60       ' Ctrl F3 should be
        '' &h61       ' Ctrl F4 should be
        '' &h62       ' Ctrl F5 should be 
        '' &h63       ' Ctrl F6 should be
        SC_ALTGR      = 56 ''  = SC_ALT
 end enum

# if __FB_LANG__ = "fb"
end namespace
# endif

#endif
