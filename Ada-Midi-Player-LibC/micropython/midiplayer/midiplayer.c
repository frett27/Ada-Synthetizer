// Include MicroPython API.
#include "py/runtime.h"

// This is the function which will be called from Python as cexample.add_ints(a, b).
STATIC mp_obj_t midiplayer_add_ints(mp_obj_t a_obj, mp_obj_t b_obj) {
    // Extract the ints from the micropython input objects.
    int a = mp_obj_get_int(a_obj);
    int b = mp_obj_get_int(b_obj);

    // Calculate the addition and convert to MicroPython object.
    return mp_obj_new_int(a + b);
}


STATIC mp_obj_t midiplayer_init(mp_obj_t filename) {
//	const char * sFileName = mp_obj_str_get_str(filename); 
	
	return MP_OBJ_NULL;
}

/*
 -- Init
   function Init return API_RETURN_CODE;
   
   -- Load_SoundBank
   function Load_SoundBank(FileName: C.Strings.chars_ptr) return SoundBankRef;
   
   --  define the sound bank playing
   function Define_SoundBank (SoundBank: SoundBankRef) return API_RETURN_CODE;

   --  play the given midi filename
   function Play (FileName : C.Strings.chars_ptr) return API_RETURN_CODE;

   -- change tempo factor
   function Change_Tempo_Factor (Tempo_Factor : Float) return API_RETURN_CODE;

   -- activate bank
   function Activate_Bank (Bank_Name : C.Strings.chars_ptr) return API_RETURN_CODE;

   -- deactivate bank
   function Deactivate_Bank (Bank_Name : C.Strings.chars_ptr) return API_RETURN_CODE;



   --  is it playing ?
   function IsPlaying return Natural;

   --  stop the play, release
   function Stop return API_RETURN_CODE;
*/






// Define a Python reference to the function above.
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_obj, midiplayer_init);

// Define all properties of the module.
// Table entries are key/value pairs of the attribute name (a string)
// and the MicroPython object reference.
// All identifiers and strings are written as MP_QSTR_xxx and will be
// optimized to word-sized integers by the build system (interned strings).
STATIC const mp_rom_map_elem_t midiplayer_module_globals_table[] = {
    { MP_ROM_QSTR(MP_QSTR___name__), MP_ROM_QSTR(MP_QSTR_midiplayer) },
    { MP_ROM_QSTR(MP_QSTR_init), MP_ROM_PTR(&midiplayer_obj) },
};
STATIC MP_DEFINE_CONST_DICT(midiplayer_module_globals, midiplayer_module_globals_table);

// Define module object.
const mp_obj_module_t midiplayer_user_cmodule = {
    .base = { &mp_type_module },
    .globals = (mp_obj_dict_t *)&midiplayer_module_globals,
};

// Register the module to make it available in Python.
MP_REGISTER_MODULE(MP_QSTR_midiplayer, midiplayer_user_cmodule, MODULE_MIDIPLAYER_ENABLED);
