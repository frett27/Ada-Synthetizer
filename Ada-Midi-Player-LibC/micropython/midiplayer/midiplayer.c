// Include MicroPython API.
#include "py/runtime.h"

extern void midiplayerlibcinit();

extern int midiplayerlib_init(void);
extern void *midiplayerlib_loadsoundbank(const char* filename);
extern int midiplayerlib_play(const char* filename);
extern int midiplayerlib_pauseresume(void); // return -1, 0: false, 1:true
extern int midiplayerlib_stop(void);
extern int midiplayerlib_stopallvoices(void);
extern int midiplayerlib_definesoundbank(const void* soundbank);
extern int midiplayerlib_activatebank(const char* bank);
extern int midiplayerlib_deactivatebank(const char* bank);
extern int midiplayerlib_changetempofactor(const double f);

extern float midiplayerlib_play_currentstreamposition();
extern float midiplayerlib_play_currentstreamlength();
extern int midiplayerlib_isplaying(); // 1 true , 0 false

extern int midiplayerlib_activatefeature(const char* feature, int activated);

/**
 * Init the synthetizer
 */ 
STATIC mp_obj_t midiplayer_init() {
    midiplayerlibcinit();
    int r = midiplayerlib_init();
    if (r != 0) {
        // raise py exception
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Init_Failed));
    }
	return mp_const_none;
}


STATIC mp_obj_t midiplayer_activatefeature(mp_obj_t feature, mp_obj_t activated) {
	const char * sfeature = mp_obj_str_get_str(feature); 
    const int act = mp_obj_int_get_checked(activated);

	int resultCode = midiplayerlib_activatefeature(sfeature, act);
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Fail_to_change_feature));
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
	int resultCode = midiplayerlib_definesoundbank(currentsb);
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Error_define_sound_bank));
    }
	return mp_const_none;
}

STATIC mp_obj_t midiplayer_activatebank(mp_obj_t bank) {
    const char * sbank = mp_obj_str_get_str(bank); 
	int resultCode = midiplayerlib_activatebank(sbank);
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
	return mp_const_none;
}

STATIC mp_obj_t midiplayer_deactivatebank(mp_obj_t bank) {
    const char * sbank = mp_obj_str_get_str(bank); 
	int resultCode = midiplayerlib_deactivatebank(sbank);
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
	return mp_const_none;
}


STATIC mp_obj_t midiplayer_stop() {
    int resultCode = midiplayerlib_stop();
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
    return mp_const_none;
}


STATIC mp_obj_t midiplayer_stop_all_voices() {
    int resultCode = midiplayerlib_stopallvoices();
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
    return mp_const_none;
}


STATIC mp_obj_t midiplayer_play(mp_obj_t filename) {
	const char * sFileName = mp_obj_str_get_str(filename); 
    int resultCode = midiplayerlib_play(sFileName);
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_Play_Failed));
    }
    return mp_const_none;
}

STATIC mp_obj_t midiplayer_changetempofactor(mp_obj_t tempo) {
	double tempofactor = mp_obj_float_get(tempo); 
    int resultCode = midiplayerlib_changetempofactor(tempofactor);
    if (resultCode != 0) {
        mp_raise_ValueError(MP_ROM_QSTR(MP_QSTR_api_failed));
    }
    return mp_const_none;
}

STATIC mp_obj_t midiplayer_currentstreamposition() {
    float r = midiplayerlib_play_currentstreamposition();
    return mp_obj_new_float(r);
}

STATIC mp_obj_t midiplayer_currentstreamlength() {
    float r = midiplayerlib_play_currentstreamlength();
    return mp_obj_new_float(r);
}

STATIC mp_obj_t midiplayer_isplaying() {
    int r = midiplayerlib_isplaying();
    return mp_obj_new_int(r);
}


STATIC mp_obj_t midiplayer_pause_resume() {
    int r = midiplayerlib_pauseresume();
    return mp_obj_new_int(r);
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
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_stop_obj, midiplayer_stop);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_definesoundbank_obj, midiplayer_definesoundbank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_activatebank_obj, midiplayer_activatebank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_deactivatebank_obj, midiplayer_deactivatebank);
STATIC MP_DEFINE_CONST_FUN_OBJ_1(midiplayer_changetempofactor_obj, midiplayer_changetempofactor);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_currentstreamlength_obj, midiplayer_currentstreamlength);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_currentstreampos_obj, midiplayer_currentstreamposition);
STATIC MP_DEFINE_CONST_FUN_OBJ_2(midiplayer_activatefeature_obj, midiplayer_activatefeature);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_isplaying_obj, midiplayer_isplaying);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_pause_resume_obj, midiplayer_pause_resume);
STATIC MP_DEFINE_CONST_FUN_OBJ_0(midiplayer_stop_all_voices_obj, midiplayer_stop_all_voices);



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
    { MP_ROM_QSTR(MP_QSTR_stop), MP_ROM_PTR(&midiplayer_stop_obj) },
    { MP_ROM_QSTR(MP_QSTR_definesoundbank), MP_ROM_PTR(&midiplayer_definesoundbank_obj) },
    { MP_ROM_QSTR(MP_QSTR_activatebank), MP_ROM_PTR(&midiplayer_activatebank_obj) },
    { MP_ROM_QSTR(MP_QSTR_deactivatebank), MP_ROM_PTR(&midiplayer_deactivatebank_obj) },
    { MP_ROM_QSTR(MP_QSTR_changetempofactor), MP_ROM_PTR(&midiplayer_changetempofactor_obj) },
    { MP_ROM_QSTR(MP_QSTR_playstreamlength), MP_ROM_PTR(&midiplayer_currentstreamlength_obj) },
    { MP_ROM_QSTR(MP_QSTR_playstreamposition), MP_ROM_PTR(&midiplayer_currentstreampos_obj) },
    { MP_ROM_QSTR(MP_QSTR_feature_set), MP_ROM_PTR(&midiplayer_activatefeature_obj) },
    { MP_ROM_QSTR(MP_QSTR_stopallvoices), MP_ROM_PTR(&midiplayer_stop_all_voices_obj) },
    { MP_ROM_QSTR(MP_QSTR_isplaying), MP_ROM_PTR(&midiplayer_isplaying_obj) },
    { MP_ROM_QSTR(MP_QSTR_pauseresume), MP_ROM_PTR(&midiplayer_pause_resume_obj) },
    
};
STATIC MP_DEFINE_CONST_DICT(midiplayer_module_globals, midiplayer_module_globals_table);

// Define module object.
const mp_obj_module_t midiplayer_cmodule = {
    .base = { &mp_type_module },
    .globals = (mp_obj_dict_t *)&midiplayer_module_globals,
};

// Register the module to make it available in Python.
MP_REGISTER_MODULE(MP_QSTR_midiplayer, midiplayer_cmodule, MODULE_MIDIPLAYER_ENABLED);



