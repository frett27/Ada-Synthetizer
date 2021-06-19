// Include MicroPython API.
#include "py/runtime.h"

extern void midiplayerlibcinit();

extern int midiplayerlib_init(void);
extern void *midiplayerlib_loadsoundbank(const char* filename);
extern int midiplayerlib_play(const char* filename);
extern int midiplayerlib_definesoundbank(const void* soundbank);
extern int midiplayerlib_activatebank(const char* bank);
extern int midiplayerlib_deactivatebank(const char* bank);
extern int midiplayerlib_changetempofactor(const double f);


STATIC mp_obj_t midiplayer_init() {
    midiplayerlibcinit();
    int r = midiplayerlib_init();
    if (r != 0) {
        // raise py exception
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Init_Failed));
    }
	return mp_const_none;
}

static void* currentsb = NULL;

STATIC mp_obj_t midiplayer_loadsoundbank(mp_obj_t filename) {
	const char * sFileName = mp_obj_str_get_str(filename); 
	currentsb = midiplayerlib_loadsoundbank(sFileName);
    if (currentsb == NULL) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Soundbank_load_failed));
    }
	return mp_const_none;
}

STATIC mp_obj_t midiplayer_definesoundbank() {
    if (currentsb == NULL) {
         mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_No_loaded_bank));
         return mp_const_none;
    }
	int r = midiplayerlib_definesoundbank(currentsb);
    if (r != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Error_define_sound_bank));
    }
	return mp_const_none;
}

STATIC mp_obj_t midiplayer_activatebank(mp_obj_t bank) {
    const char * sbank = mp_obj_str_get_str(bank); 
	int r = midiplayerlib_activatebank(sbank);
    if (r != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
	return mp_const_none;
}

STATIC mp_obj_t midiplayer_deactivatebank(mp_obj_t bank) {
    const char * sbank = mp_obj_str_get_str(bank); 
	int r = midiplayerlib_deactivatebank(sbank);
    if (r != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
	return mp_const_none;
}


STATIC mp_obj_t midiplayer_play(mp_obj_t filename) {
	const char * sFileName = mp_obj_str_get_str(filename); 
    int r = midiplayerlib_play(sFileName);
    if (r != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Play_Failed));
    }
    return mp_const_none;
}

STATIC mp_obj_t midiplayer_changetempofactor(mp_obj_t tempo) {
	double tempofactor = mp_obj_float_get(tempo); 
    int r = midiplayerlib_changetempofactor(tempofactor);
    if (r != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
    return mp_const_none;
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
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_init_obj, midiplayer_init);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_loadsoundbank_obj, midiplayer_loadsoundbank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_play_obj, midiplayer_play);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_definesoundbank_obj, midiplayer_definesoundbank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_activatebank_obj, midiplayer_activatebank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_deactivatebank_obj, midiplayer_deactivatebank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_changetempofactor_obj, midiplayer_changetempofactor);



// Define all properties of the module.
// Table entries are key/value pairs of the attribute name (a string)
// and the MicroPython object reference.
// All identifiers and strings are written as MP_QSTR_xxx and will be
// optimized to word-sized integers by the build system (interned strings).
STATIC const mp_rom_map_elem_t midiplayer_module_globals_table[] = {
    { MP_ROM_QSTR(MP_QSTR___name__), MP_ROM_QSTR(MP_QSTR_midiplayer) },
    { MP_ROM_QSTR(MP_QSTR_init), MP_ROM_PTR(&midiplayer_init_obj) },
    { MP_ROM_QSTR(MP_QSTR_loadsoundbank), MP_ROM_PTR(&midiplayer_loadsoundbank_obj) },
    { MP_ROM_QSTR(MP_QSTR_play), MP_ROM_PTR(&midiplayer_play_obj) },
    { MP_ROM_QSTR(MP_QSTR_definesoundbank), MP_ROM_PTR(&midiplayer_definesoundbank_obj) },
    { MP_ROM_QSTR(MP_QSTR_activatebank), MP_ROM_PTR(&midiplayer_activatebank_obj) },
    { MP_ROM_QSTR(MP_QSTR_deactivatebank), MP_ROM_PTR(&midiplayer_deactivatebank_obj) },
    { MP_ROM_QSTR(MP_QSTR_changetempofactor), MP_ROM_PTR(&midiplayer_changetempofactor_obj) },
};
STATIC MP_DEFINE_CONST_DICT(midiplayer_module_globals, midiplayer_module_globals_table);

// Define module object.
const mp_obj_module_t midiplayer_user_cmodule = {
    .base = { &mp_type_module },
    .globals = (mp_obj_dict_t *)&midiplayer_module_globals,
};

// Register the module to make it available in Python.
MP_REGISTER_MODULE(MP_QSTR_midiplayer, midiplayer_user_cmodule, MODULE_MIDIPLAYER_ENABLED);

