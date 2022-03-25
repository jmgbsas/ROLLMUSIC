''
''
'' rtmidi_c -- header translated with help of SWIG FB wrapper
''
'' NOTICE: This file is part of the FreeBASIC Compiler package and can't
''         be included in other distributions without authorization.
''
''
#ifndef __rtmidi_c_bi__
#define __rtmidi_c_bi__
#define __RTMIDI_DEBUG__
type RtMidiWrapper
	ptr as any ptr
	data as any ptr
	ok as integer
	msg as zstring ptr
end type

type RtMidiPtr as RtMidiWrapper ptr
type RtMidiInPtr as RtMidiWrapper ptr
type RtMidiOutPtr as RtMidiWrapper ptr

enum RtMidiApi
	RTMIDI_API_UNSPECIFIED
	RTMIDI_API_MACOSX_CORE
	RTMIDI_API_LINUX_ALSA
	RTMIDI_API_UNIX_JACK
	RTMIDI_API_WINDOWS_MM
	RTMIDI_API_RTMIDI_DUMMY
	RTMIDI_API_NUM
end enum

enum RtMidiErrorType
	RTMIDI_ERROR_WARNING
	RTMIDI_ERROR_DEBUG_WARNING
	RTMIDI_ERROR_UNSPECIFIED
	RTMIDI_ERROR_NO_DEVICES_FOUND
	RTMIDI_ERROR_INVALID_DEVICE
	RTMIDI_ERROR_MEMORY_ERROR
	RTMIDI_ERROR_INVALID_PARAMETER
	RTMIDI_ERROR_INVALID_USE
	RTMIDI_ERROR_DRIVER_ERROR
	RTMIDI_ERROR_SYSTEM_ERROR
	RTMIDI_ERROR_THREAD_ERROR
end enum

type RtMidiCCallback as sub cdecl(byval as double, byval as ubyte ptr, byval as UInteger<64>, byval as any ptr)

declare function rtmidi_get_compiled_api cdecl alias "rtmidi_get_compiled_api" (byval apis as RtMidiApi ptr, byval apis_size as uinteger) as integer
declare function rtmidi_api_name cdecl alias "rtmidi_api_name" (byval api as RtMidiApi) as zstring ptr
declare function rtmidi_api_display_name cdecl alias "rtmidi_api_display_name" (byval api as RtMidiApi) as zstring ptr
declare function rtmidi_compiled_api_by_name cdecl alias "rtmidi_compiled_api_by_name" (byval name as zstring ptr) as RtMidiApi
declare      sub rtmidi_error cdecl alias "rtmidi_error" (byval type as RtMidiErrorType, byval errorString as zstring ptr)
declare      sub open_port cdecl alias "rtmidi_open_port" (byval device as RtMidiPtr, byval portNumber as uinteger, byval portName as zstring ptr)
declare      sub rtmidi_open_virtual_port cdecl alias "rtmidi_open_virtual_port" (byval device as RtMidiPtr, byval portName as zstring ptr)
declare      sub close_port cdecl alias "rtmidi_close_port" (byval device as RtMidiPtr)
declare function port_count cdecl alias "rtmidi_get_port_count" (byval device as RtMidiPtr) as uinteger
declare function port_name cdecl alias "rtmidi_get_port_name" (byval device as RtMidiPtr, byval portNumber as uinteger) as zstring ptr
declare function rtmidi_in_create_default cdecl alias "rtmidi_in_create_default" () as RtMidiInPtr
declare function rtmidi_in_create cdecl alias "rtmidi_in_create" (byval api as RtMidiApi, byval clientName as zstring ptr, byval queueSizeLimit as uinteger) as RtMidiInPtr
declare      sub rtmidi_in_free cdecl alias "rtmidi_in_free" (byval device as RtMidiInPtr)
declare function rtmidi_in_get_current_api cdecl alias "rtmidi_in_get_current_api" (byval device as RtMidiPtr) as RtMidiApi
Declare     sub rtmidi_in_set_callback cdecl alias "rtmidi_in_set_callback" (byval device as RtMidiInPtr, byval callback as RtMidiCCallback, byval userData as any ptr)
declare      sub rtmidi_in_cancel_callback cdecl alias "rtmidi_in_cancel_callback" (byval device as RtMidiInPtr)
declare      sub rtmidi_in_ignore_types cdecl alias "rtmidi_in_ignore_types" (byval device as RtMidiInPtr, byval midiSysex as integer, byval midiTime as integer, byval midiSense as integer)
declare function get_message cdecl alias "rtmidi_in_get_message" (byval device as RtMidiInPtr, byval message as ubyte ptr, byval size as UInteger<64> ptr) as double
declare function rtmidi_out_create_default cdecl alias "rtmidi_out_create_default" () as RtMidiOutPtr
declare function rtmidi_out_create cdecl alias "rtmidi_out_create" (byval api as RtMidiApi, byval clientName as zstring ptr) as RtMidiOutPtr
declare      sub out_free cdecl alias "rtmidi_out_free" (byval device as RtMidiOutPtr)
declare function rtmidi_out_get_current_api cdecl alias "rtmidi_out_get_current_api" (byval device as RtMidiPtr) as RtMidiApi
declare function send_message cdecl alias "rtmidi_out_send_message" (byval device as RtMidiOutPtr, byval message as ubyte ptr, byval length as integer) as integer

#endif
