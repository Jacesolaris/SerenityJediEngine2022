/*
===========================================================================
Copyright (C) 1999 - 2005, Id Software, Inc.
Copyright (C) 2000 - 2013, Raven Software, Inc.
Copyright (C) 2001 - 2013, Activision, Inc.
Copyright (C) 2005 - 2015, ioquake3 contributors
Copyright (C) 2013 - 2015, OpenJK contributors

This file is part of the OpenJK source code.

OpenJK is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License version 2 as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>.
===========================================================================
*/

// cvar.c -- dynamic variable tracking

#include "qcommon/qcommon.h"

cvar_t* cvar_vars = nullptr;
cvar_t* cvar_cheats;

uint32_t	cvar_modifiedFlags;

#define	MAX_CVARS	8192
cvar_t		cvar_indexes[MAX_CVARS];
int			cvar_numIndexes;

#define FILE_HASH_SIZE		512
static	cvar_t* hashTable[FILE_HASH_SIZE];
static	qboolean cvar_sort = qfalse;

static char* lastMemPool = nullptr;
static int memPoolSize;

//If the string came from the memory pool, don't really free it.  The entire
//memory pool will be wiped during the next level load.
static void Cvar_FreeString(char* string)
{
	if (!lastMemPool || string < lastMemPool ||
		string >= lastMemPool + memPoolSize) {
		Z_Free(string);
	}
}

/*
================
return a hash value for the filename
================
*/
static long generateHashValue(const char* fname) {
	long hash = 0;
	int i = 0;
	while (fname[i] != '\0') {
		const char letter = tolower(static_cast<unsigned char>(fname[i]));
		hash += static_cast<long>(letter) * (i + 119);
		i++;
	}
	hash &= FILE_HASH_SIZE - 1;
	return hash;
}

/*
============
Cvar_ValidateString
============
*/
static qboolean Cvar_ValidateString(const char* s) {
	if (!s) {
		return qfalse;
	}
	if (strchr(s, '\\')) {
		return qfalse;
	}
	if (strchr(s, '\"')) {
		return qfalse;
	}
	if (strchr(s, ';')) {
		return qfalse;
	}
	return qtrue;
}

/*
============
Cvar_FindVar
============
*/
static cvar_t* Cvar_FindVar(const char* var_name) {
	const long hash = generateHashValue(var_name);

	for (cvar_t* var = hashTable[hash]; var; var = var->hashNext) {
		if (!Q_stricmp(var_name, var->name)) {
			return var;
		}
	}

	return nullptr;
}

/*
============
Cvar_VariableValue
============
*/
float Cvar_VariableValue(const char* var_name) {
	const cvar_t* var = Cvar_FindVar(var_name);
	if (!var)
		return 0;
	return var->value;
}

/*
============
Cvar_VariableIntegerValue
============
*/
int Cvar_VariableIntegerValue(const char* var_name) {
	const cvar_t* var = Cvar_FindVar(var_name);
	if (!var)
		return 0;
	return var->integer;
}

/*
============
Cvar_VariableString
============
*/
char* Cvar_VariableString(const char* var_name) {
	const cvar_t* var = Cvar_FindVar(var_name);
	if (!var)
		return "";
	return var->string;
}

/*
============
Cvar_VariableStringBuffer
============
*/
void Cvar_VariableStringBuffer(const char* var_name, char* buffer, int bufsize) {
	const cvar_t* var = Cvar_FindVar(var_name);
	if (!var) {
		*buffer = 0;
	}
	else {
		Q_strncpyz(buffer, var->string, bufsize);
	}
}

/*
============
Cvar_DescriptionString
============
*/
char* Cvar_DescriptionString(const char* var_name)
{
	const cvar_t* var = Cvar_FindVar(var_name);
	if (!var || !VALIDSTRING(var->description))
		return "";
	return var->description;
}

/*
============
Cvar_Flags
============
*/
uint32_t Cvar_Flags(const char* var_name) {
	cvar_t* var;

	if (!(var = Cvar_FindVar(var_name)))
		return CVAR_NONEXISTENT;
	if (var->modified)
		return var->flags | CVAR_MODIFIED;
	return var->flags;
}

/*
============
Cvar_CommandCompletion
============
*/
void	Cvar_CommandCompletion(callbackFunc_t callback) {
	for (const cvar_t* cvar = cvar_vars; cvar; cvar = cvar->next) {
		// Don't show internal cvars
		if (cvar->flags & CVAR_INTERNAL)
		{
			continue;
		}
		callback(cvar->name);
	}
}

/*
============
Cvar_Validate
============
*/
static const char* Cvar_Validate(cvar_t* var, const char* value, qboolean warn)
{
	static char s[MAX_CVAR_VALUE_STRING];
	float valuef;
	qboolean changed = qfalse;

	if (!var->validate)
		return value;

	if (!value)
		return value;

	if (Q_isanumber(value))
	{
		valuef = atof(value);

		if (var->integral)
		{
			if (!Q_isintegral(valuef))
			{
				if (warn)
					Com_Printf("WARNING: cvar '%s' must be integral", var->name);

				valuef = static_cast<int>(valuef);
				changed = qtrue;
			}
		}
	}
	else
	{
		if (warn)
			Com_Printf("WARNING: cvar '%s' must be numeric", var->name);

		valuef = atof(var->resetString);
		changed = qtrue;
	}

	if (valuef < var->min)
	{
		if (warn)
		{
			if (changed)
				Com_Printf(" and is");
			else
				Com_Printf("WARNING: cvar '%s'", var->name);

			if (Q_isintegral(var->min))
				Com_Printf(" out of range (min %d)", static_cast<int>(var->min));
			else
				Com_Printf(" out of range (min %f)", var->min);
		}

		valuef = var->min;
		changed = qtrue;
	}
	else if (valuef > var->max)
	{
		if (warn)
		{
			if (changed)
				Com_Printf(" and is");
			else
				Com_Printf("WARNING: cvar '%s'", var->name);

			if (Q_isintegral(var->max))
				Com_Printf(" out of range (max %d)", static_cast<int>(var->max));
			else
				Com_Printf(" out of range (max %f)", var->max);
		}

		valuef = var->max;
		changed = qtrue;
	}

	if (changed)
	{
		if (Q_isintegral(valuef))
		{
			Com_sprintf(s, sizeof s, "%d", static_cast<int>(valuef));

			if (warn)
				Com_Printf(", setting to %d\n", static_cast<int>(valuef));
		}
		else
		{
			Com_sprintf(s, sizeof s, "%f", valuef);

			if (warn)
				Com_Printf(", setting to %f\n", valuef);
		}

		return s;
	}
	return value;
}

/*
============
Cvar_Get

If the variable already exists, the value will not be set unless CVAR_ROM
The flags will be or'ed in if the variable exists.
============
*/
cvar_t* Cvar_Get(const char* var_name, const char* var_value, uint32_t flags, const char* var_desc) {
	cvar_t* var;
	long	hash;
	int		index;

	if (!var_name || !var_value) {
		Com_Error(ERR_FATAL, "Cvar_Get: NULL parameter");
	}

	if (!Cvar_ValidateString(var_name)) {
		Com_Printf("invalid cvar name string: %s\n", var_name);
		var_name = "BADNAME";
	}

#if 0		// FIXME: values with backslash happen
	if (!Cvar_ValidateString(var_value)) {
		Com_Printf("invalid cvar value string: %s\n", var_value);
		var_value = "BADVALUE";
	}
#endif

	var = Cvar_FindVar(var_name);
	if (var) {
		var_value = Cvar_Validate(var, var_value, qfalse);

		// Make sure the game code cannot mark engine-added variables as gamecode vars
		if (var->flags & CVAR_VM_CREATED)
		{
			if (!(flags & CVAR_VM_CREATED))
				var->flags &= ~CVAR_VM_CREATED;
		}
		else if (!(var->flags & CVAR_USER_CREATED))
		{
			if (flags & CVAR_VM_CREATED)
				flags &= ~CVAR_VM_CREATED;
		}

		// if the C code is now specifying a variable that the user already
		// set a value for, take the new value as the reset value
		if (var->flags & CVAR_USER_CREATED)
		{
			var->flags &= ~CVAR_USER_CREATED;
			Cvar_FreeString(var->resetString);
			var->resetString = CopyString(var_value);

			if (flags & CVAR_ROM)
			{
				// this variable was set by the user,
				// so force it to value given by the engine.

				if (var->latchedString)
					Cvar_FreeString(var->latchedString);

				var->latchedString = CopyString(var_value);
			}
		}

		// Make sure servers cannot mark engine-added variables as SERVER_CREATED
		if (var->flags & CVAR_SERVER_CREATED)
		{
			if (!(flags & CVAR_SERVER_CREATED))
				var->flags &= ~CVAR_SERVER_CREATED;
		}
		else
		{
			if (flags & CVAR_SERVER_CREATED)
				flags &= ~CVAR_SERVER_CREATED;
		}

		var->flags |= flags;

		// only allow one non-empty reset string without a warning
		if (!var->resetString[0]) {
			// we don't have a reset string yet
			Cvar_FreeString(var->resetString);
			var->resetString = CopyString(var_value);
		}
		else if (var_value[0] && strcmp(var->resetString, var_value) != 0) {
			Com_DPrintf(S_COLOR_YELLOW "Warning: cvar \"%s\" given initial values: \"%s\" and \"%s\"\n",
				var_name, var->resetString, var_value);
		}
		// if we have a latched string, take that value now
		if (var->latchedString) {
			char* s = var->latchedString;
			var->latchedString = nullptr;	// otherwise cvar_set2 would free it
			Cvar_Set2(var_name, s, 0, qtrue);
			Cvar_FreeString(s);
		}

		if (var_desc && var_desc[0] != '\0')
		{
			if (var->description)
				Cvar_FreeString(var->description);
			var->description = CopyString(var_desc);
		}

		// ZOID--needs to be set so that cvars the game sets as
		// SERVERINFO get sent to clients
		cvar_modifiedFlags |= flags;

		return var;
	}

	//
	// allocate a new cvar
	//

	// find a free cvar
	for (index = 0; index < MAX_CVARS; index++)
	{
		if (!cvar_indexes[index].name)
			break;
	}

	if (index >= MAX_CVARS)
	{
		if (!com_errorEntered)
			Com_Error(ERR_FATAL, "Error: Too many cvars, cannot create a new one!");

		return nullptr;
	}

	var = &cvar_indexes[index];

	if (index >= cvar_numIndexes)
		cvar_numIndexes = index + 1;

	var->name = CopyString(var_name);
	var->string = CopyString(var_value);
	if (var_desc && var_desc[0] != '\0')
		var->description = CopyString(var_desc);
	else
		var->description = nullptr;
	var->modified = qtrue;
	var->modificationCount = 1;
	var->value = atof(var->string);
	var->integer = atoi(var->string);
	var->resetString = CopyString(var_value);
	var->validate = qfalse;

	// link the variable in
	var->next = cvar_vars;
	if (cvar_vars)
		cvar_vars->prev = var;

	var->prev = nullptr;
	cvar_vars = var;

	var->flags = flags;
	// note what types of cvars have been modified (userinfo, archive, serverinfo, systeminfo)
	cvar_modifiedFlags |= var->flags;

	hash = generateHashValue(var_name);
	var->hashIndex = hash;

	var->hashNext = hashTable[hash];
	if (hashTable[hash])
		hashTable[hash]->hashPrev = var;

	var->hashPrev = nullptr;
	hashTable[hash] = var;

	// sort on write
	cvar_sort = qtrue;

	return var;
}

static void Cvar_QSortByName(cvar_t** a, int n)
{
	int i = 0;
	int j = n;
	const cvar_t* m = a[n >> 1];

	do {
		// sort in descending order
		while (strcmp(a[i]->name, m->name) > 0) i++;
		while (strcmp(a[j]->name, m->name) < 0) j--;

		if (i <= j) {
			cvar_t* temp = a[i];
			a[i] = a[j];
			a[j] = temp;
			i++;
			j--;
		}
	} while (i <= j);

	if (j > 0) Cvar_QSortByName(a, j);
	if (n > i) Cvar_QSortByName(a + i, n - i);
}

static void Cvar_Sort(void)
{
	cvar_t* list[MAX_CVARS], * var;
	int count;

	for (count = 0, var = cvar_vars; var; var = var->next) {
		if (var->name) {
			list[count++] = var;
		}
		else {
			Com_Error(ERR_FATAL, "Cvar_Sort: NULL cvar name");
		}
	}

	if (count < 2) {
		return; // nothing to sort
	}

	Cvar_QSortByName(&list[0], count - 1);

	cvar_vars = nullptr;

	// relink cvars
	for (int i = 0; i < count; i++) {
		var = list[i];
		// link the variable in
		var->next = cvar_vars;
		if (cvar_vars)
			cvar_vars->prev = var;
		var->prev = nullptr;
		cvar_vars = var;
	}
}

/*
============
Cvar_Print

Prints the value, default, and latched string of the given variable
============
*/
void Cvar_Print(cvar_t* v) {
	Com_Printf(S_COLOR_GREY "Cvar " S_COLOR_WHITE "%s = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE, v->name, v->string);

	if (!(v->flags & CVAR_ROM)) {
		if (!Q_stricmp(v->string, v->resetString))
			Com_Printf(", " S_COLOR_WHITE "the default");
		else
			Com_Printf(", " S_COLOR_WHITE "default = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE, v->resetString);
	}

	Com_Printf("\n");

	if (v->latchedString)
		Com_Printf("     latched = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"\n", v->latchedString);

	if (v->description)
		Com_Printf("%s\n", v->description);
}

/*
============
Cvar_Set2
============
*/
cvar_t* Cvar_Set2(const char* var_name, const char* value, uint32_t defaultFlags, qboolean force) {
	cvar_t* var;

	if (!Cvar_ValidateString(var_name)) {
		Com_Printf("invalid cvar name string: %s\n", var_name);
		var_name = "BADNAME";
	}

#if 0	// FIXME
	if (value && !Cvar_ValidateString(value)) {
		Com_Printf("invalid cvar value string: %s\n", value);
		var_value = "BADVALUE";
	}
#endif

	var = Cvar_FindVar(var_name);
	if (!var) {
		if (!value) {
			return nullptr;
		}
		// create it
		return Cvar_Get(var_name, value, defaultFlags);
	}

	if (!value) {
		value = var->resetString;
	}

	value = Cvar_Validate(var, value, qtrue);

	if (var->flags & CVAR_LATCH && var->latchedString)
	{
		if (strcmp(value, var->string) == 0)
		{
			Cvar_FreeString(var->latchedString);
			var->latchedString = nullptr;
			return var;
		}

		if (strcmp(value, var->latchedString) == 0)
			return var;
	}
	else if (strcmp(value, var->string) == 0)
		return var;

	// note what types of cvars have been modified (userinfo, archive, serverinfo, systeminfo)
	cvar_modifiedFlags |= var->flags;

	if (!force)
	{
		if (var->flags & (CVAR_SYSTEMINFO | CVAR_SERVER_CREATED) && CL_ConnectedToRemoteServer())
		{
			Com_Printf("%s can only be set by server.\n", var_name);
			return var;
		}

		if (var->flags & CVAR_ROM)
		{
			Com_Printf("%s is read only.\n", var_name);
			return var;
		}

		if (var->flags & CVAR_INIT)
		{
			Com_Printf("%s is write protected.\n", var_name);
			return var;
		}

		if (var->flags & CVAR_LATCH)
		{
			if (var->latchedString)
			{
				if (strcmp(value, var->latchedString) == 0)
					return var;
				Cvar_FreeString(var->latchedString);
			}
			else
			{
				if (strcmp(value, var->string) == 0)
					return var;
			}

			Com_Printf("%s will be changed upon restarting.\n", var_name);
			var->latchedString = CopyString(value);
			var->modified = qtrue;
			var->modificationCount++;
			return var;
		}

		if (var->flags & CVAR_CHEAT && !cvar_cheats->integer)
		{
			Com_Printf("%s is cheat protected.\n", var_name);
			return var;
		}
	}
	else
	{
		if (var->latchedString)
		{
			Cvar_FreeString(var->latchedString);
			var->latchedString = nullptr;
		}
	}

	if (strcmp(value, var->string) == 0)
		return var;		// not changed

	var->modified = qtrue;
	var->modificationCount++;

	Cvar_FreeString(var->string);	// free the old value string

	var->string = CopyString(value);
	var->value = atof(var->string);
	var->integer = atoi(var->string);

	return var;
}

/*
============
Cvar_Set

Force cvar to a value
============
*/
cvar_t* Cvar_Set(const char* var_name, const char* value) {
	return Cvar_Set2(var_name, value, 0, qtrue);
}

/*
============
Cvar_SetSafe

Try to set cvar to a value. respects CVAR_ROM, etc.
============
*/
cvar_t* Cvar_SetSafe(const char* var_name, const char* value) {
	return Cvar_Set2(var_name, value, 0, qfalse);
}

/*
============
Cvar_User_Set

Same as Cvar_SetSafe, but have new cvars have user created flag.
============
*/
cvar_t* Cvar_User_Set(const char* var_name, const char* value) {
	return Cvar_Set2(var_name, value, CVAR_USER_CREATED, qfalse);
}

static const char* legacyCvars[] = {
	"bg_fighterAltControl",
	"g_dlURL",
	"g_synchronousClients",
	"jp_DlBaseURL",
	"pmove_fixed",
	"pmove_float",
	"pmove_msec",
	"vm_cgame",
	"vm_game",
	"vm_ui"
};

static const size_t numLegacyCvars = ARRAY_LEN(legacyCvars);

static bool FindLegacyCvar(const char* var_name) {
	for (auto& legacyCvar : legacyCvars) {
		if (!Q_stricmp(legacyCvar, var_name))
			return true;
	}
	return false;
}

/*
============
Cvar_Server_Set

Set cvars from network server.
============
*/
void Cvar_Server_Set(const char* var_name, const char* value)
{
	const uint32_t flags = Cvar_Flags(var_name);

	if (flags != CVAR_NONEXISTENT) {
		// If this cvar may not be modified by a server discard the value.
		// But check for ones where the legacy gamecode might still need to be allowed
		if (!(flags & (CVAR_SYSTEMINFO | CVAR_SERVER_CREATED | CVAR_USER_CREATED)))
		{
			if (!FindLegacyCvar(var_name)) {
				Com_Printf(S_COLOR_YELLOW "WARNING: server is not allowed to set %s=%s\n", var_name, value);
				return;
			}
		}

		if (flags & CVAR_PROTECTED)
		{
			if (value)
				Com_Error(ERR_DROP, R"(Server tried to set "%s" to "%s")", var_name, value);
			Com_Error(ERR_DROP, "Server tried to modify \"%s\"", var_name);
		}
	}

	Cvar_Set2(var_name, value, CVAR_SERVER_CREATED, qtrue);
}

/*
============
Cvar_VM_Set

Set cvar for game, cgame, or ui vm.
============
*/
void Cvar_VM_Set(const char* var_name, const char* value, vmSlots_t vmslot)
{
	const uint32_t flags = Cvar_Flags(var_name);

	if (vmslot != VM_GAME && flags & CVAR_SYSTEMINFO && CL_ConnectedToRemoteServer())
	{
		Com_Printf("%s can only be set by server.\n", var_name);
		return;
	}

	if (flags != CVAR_NONEXISTENT && flags & CVAR_PROTECTED) {
		if (value)
			Com_Error(ERR_DROP, R"(%s tried to set "%s" to "%s")", vmStrs[vmslot], var_name, value);
		Com_Error(ERR_DROP, "%s tried to modify \"%s\"", vmStrs[vmslot], var_name);
	}

	Cvar_Set2(var_name, value, CVAR_VM_CREATED, qtrue);
}

/*
============
Cvar_SetValue
============
*/
cvar_t* Cvar_SetValue(const char* var_name, float value) {
	char	val[32];

	if (Q_isintegral(value))
		Com_sprintf(val, sizeof val, "%i", static_cast<int>(value));
	else
		Com_sprintf(val, sizeof val, "%f", value);

	return Cvar_Set(var_name, val);
}

/*
============
Cvar_User_SetValue
============
*/
void Cvar_User_SetValue(const char* var_name, float value) {
	char	val[32];

	if (Q_isintegral(value))
		Com_sprintf(val, sizeof val, "%i", static_cast<int>(value));
	else
		Com_sprintf(val, sizeof val, "%f", value);

	Cvar_User_Set(var_name, val);
}

/*
============
Cvar_VM_SetValue
============
*/
void Cvar_VM_SetValue(const char* var_name, float value, vmSlots_t vmslot) {
	char	val[32];

	if (Q_isintegral(value))
		Com_sprintf(val, sizeof val, "%i", static_cast<int>(value));
	else
		Com_sprintf(val, sizeof val, "%f", value);

	Cvar_VM_Set(var_name, val, vmslot);
}

/*
============
Cvar_Reset
============
*/
void Cvar_Reset(const char* var_name) {
	Cvar_SetSafe(var_name, nullptr);
}

/*
============
Cvar_ForceReset
============
*/
void Cvar_ForceReset(const char* var_name)
{
	Cvar_Set(var_name, nullptr);
}

/*
============
Cvar_SetCheatState

Any testing variables will be reset to the safe values
============
*/
void Cvar_SetCheatState(void) {
	// set all default vars to the safe value
	for (cvar_t* var = cvar_vars; var; var = var->next) {
		if (var->flags & CVAR_CHEAT) {
			// the CVAR_LATCHED|CVAR_CHEAT vars might escape the reset here
			// because of a different var->latchedString
			if (var->latchedString)
			{
				Cvar_FreeString(var->latchedString);
				var->latchedString = nullptr;
			}
			if (strcmp(var->resetString, var->string) != 0) {
				Cvar_Set(var->name, var->resetString);
			}
		}
	}
}

/*
============
Cvar_Command

Handles variable inspection and changing from the console
============
*/
qboolean Cvar_Command(void) {
	// check variables
	cvar_t* v = Cvar_FindVar(Cmd_Argv(0));
	if (!v) {
		return qfalse;
	}

	// perform a variable print or set
	if (Cmd_Argc() == 1)
	{
		Cvar_Print(v);
		return qtrue;
	}

	// toggle
	if (strcmp(Cmd_Argv(1), "!") == 0)
	{
		// Swap the value if our command has ! in it (bind p "cg_thirdPeson !")
		Cvar_User_SetValue(v->name, !v->value);
		return qtrue;
	}

	// set the value if forcing isn't required
	Cvar_User_Set(v->name, Cmd_Args());
	return qtrue;
}

/*
============
Cvar_Print_f

Prints the contents of a cvar
(preferred over Cvar_Command where cvar names and commands conflict)
============
*/
void Cvar_Print_f(void)
{
	if (Cmd_Argc() != 2)
	{
		Com_Printf("usage: print <variable>\n");
		return;
	}

	char* name = Cmd_Argv(1);

	cvar_t* cv = Cvar_FindVar(name);

	if (cv)
		Cvar_Print(cv);
	else
		Com_Printf("Cvar %s does not exist.\n", name);
}

/*
============
Cvar_Toggle_f

Toggles a cvar for easy single key binding, optionally through a list of
given values
============
*/
void Cvar_Toggle_f(void) {
	const int c = Cmd_Argc();

	if (c < 2) {
		Com_Printf("usage: toggle <variable> [value1, value2, ...]\n");
		return;
	}

	if (c == 2) {
		Cvar_User_SetValue(Cmd_Argv(1), !Cvar_VariableValue(Cmd_Argv(1)));
		return;
	}

	if (c == 3) {
		Com_Printf("toggle: nothing to toggle to\n");
		return;
	}

	const char* curval = Cvar_VariableString(Cmd_Argv(1));

	// don't bother checking the last arg for a match since the desired
	// behaviour is the same as no match (set to the first argument)
	for (int i = 2; i + 1 < c; i++) {
		if (strcmp(curval, Cmd_Argv(i)) == 0) {
			Cvar_User_Set(Cmd_Argv(1), Cmd_Argv(i + 1));
			return;
		}
	}

	// fallback
	Cvar_User_Set(Cmd_Argv(1), Cmd_Argv(2));
}

/*
============
Cvar_Set_f

Allows setting and defining of arbitrary cvars from console, even if they
weren't declared in C code.
============
*/
void Cvar_Set_f(void) {
	const int c = Cmd_Argc();
	char* cmd = Cmd_Argv(0);

	if (c < 2) {
		Com_Printf("usage: %s <variable> <value>\n", cmd);
		return;
	}
	if (c == 2) {
		Cvar_Print_f();
		return;
	}

	cvar_t* v = Cvar_User_Set(Cmd_Argv(1), Cmd_ArgsFrom(2));
	if (!v) {
		return;
	}
	switch (cmd[3]) {
	case 'a':
		if (!(v->flags & CVAR_ARCHIVE)) {
			v->flags |= CVAR_ARCHIVE;
			cvar_modifiedFlags |= CVAR_ARCHIVE;
		}
		break;
	case 'u':
		if (!(v->flags & CVAR_USERINFO)) {
			v->flags |= CVAR_USERINFO;
			cvar_modifiedFlags |= CVAR_USERINFO;
		}
		break;
	case 's':
		if (!(v->flags & CVAR_SERVERINFO)) {
			v->flags |= CVAR_SERVERINFO;
			cvar_modifiedFlags |= CVAR_SERVERINFO;
		}
		break;
	}
}

/*
============
Cvar_Math_f
============
*/
void Cvar_Math_f(void)
{
	const int c = Cmd_Argc();
	char* cmd = Cmd_Argv(0);

	if (c != 3)
	{
		Com_Printf("usage: %s <variable> <value>\n", cmd);
		return;
	}

	if (!Q_stricmp(cmd, "cvarAdd"))
	{
		Cvar_User_SetValue(Cmd_Argv(1), Cvar_VariableValue(Cmd_Argv(1)) + atof(Cmd_Argv(2)));
	}
	else if (!Q_stricmp(cmd, "cvarSub"))
	{
		Cvar_User_SetValue(Cmd_Argv(1), Cvar_VariableValue(Cmd_Argv(1)) - atof(Cmd_Argv(2)));
	}
	else if (!Q_stricmp(cmd, "cvarMult"))
	{
		Cvar_User_SetValue(Cmd_Argv(1), Cvar_VariableValue(Cmd_Argv(1)) * atof(Cmd_Argv(2)));
	}
	else if (!Q_stricmp(cmd, "cvarDiv"))
	{
		const float value = atof(Cmd_Argv(2));
		if (value != 0)
			Cvar_User_SetValue(Cmd_Argv(1), Cvar_VariableValue(Cmd_Argv(1)) / value);
		else
			Com_Printf("Cannot divide by zero!\n");
	}
	else if (!Q_stricmp(cmd, "cvarMod"))
	{
		Cvar_User_SetValue(Cmd_Argv(1), Cvar_VariableIntegerValue(Cmd_Argv(1)) % atoi(Cmd_Argv(2)));
	}
}

/*
============
Cvar_Reset_f
============
*/
void Cvar_Reset_f(void) {
	if (Cmd_Argc() != 2) {
		Com_Printf("usage: reset <variable>\n");
		return;
	}
	Cvar_Reset(Cmd_Argv(1));
}

/*
============
Cvar_WriteVariables

Appends lines containing "set variable value" for all variables
with the archive flag set to qtrue.
============
*/
void Cvar_WriteVariables(fileHandle_t f) {
	char buffer[1024];

	if (cvar_sort) {
		Com_DPrintf("Cvar_Sort: sort cvars\n");
		cvar_sort = qfalse;
		Cvar_Sort();
	}

	for (const cvar_t* var = cvar_vars; var; var = var->next)
	{
		if (!var->name || Q_stricmp(var->name, "cl_cdkey") == 0)
			continue;

		if (var->flags & CVAR_ARCHIVE) {
			// write the latched value, even if it hasn't taken effect yet
			if (var->latchedString) {
				if (strlen(var->name) + strlen(var->latchedString) + 10 > sizeof buffer) {
					Com_Printf(S_COLOR_YELLOW "WARNING: value of variable "
						"\"%s\" too long to write to file\n", var->name);
					continue;
				}
				if (var->flags & CVAR_NODEFAULT && strcmp(var->latchedString, var->resetString) == 0) {
					continue;
				}
				Com_sprintf(buffer, sizeof buffer, "seta %s \"%s\"\n", var->name, var->latchedString);
			}
			else {
				if (strlen(var->name) + strlen(var->string) + 10 > sizeof buffer) {
					Com_Printf(S_COLOR_YELLOW "WARNING: value of variable "
						"\"%s\" too long to write to file\n", var->name);
					continue;
				}
				if (var->flags & CVAR_NODEFAULT && strcmp(var->string, var->resetString) == 0) {
					continue;
				}
				Com_sprintf(buffer, sizeof buffer, "seta %s \"%s\"\n", var->name, var->string);
			}
			FS_Write(buffer, strlen(buffer), f);
		}
	}
}

/*
============
Cvar_List_f
============
*/
void Cvar_List_f(void) {
	const cvar_t* var = nullptr;
	int i = 0;
	char* match = nullptr;

	if (Cmd_Argc() > 1)
		match = Cmd_Argv(1);

	for (var = cvar_vars, i = 0;
		var;
		var = var->next, i++)
	{
		if (!var->name || match && !Com_Filter(match, var->name, qfalse))
			continue;

		if (var->flags & CVAR_SERVERINFO)	Com_Printf("S");	else Com_Printf(" ");
		if (var->flags & CVAR_SYSTEMINFO)	Com_Printf("s");	else Com_Printf(" ");
		if (var->flags & CVAR_USERINFO)		Com_Printf("U");	else Com_Printf(" ");
		if (var->flags & CVAR_ROM)			Com_Printf("R");	else Com_Printf(" ");
		if (var->flags & CVAR_INIT)			Com_Printf("I");	else Com_Printf(" ");
		if (var->flags & CVAR_ARCHIVE)		Com_Printf("A");	else Com_Printf(" ");
		if (var->flags & CVAR_LATCH)		Com_Printf("L");	else Com_Printf(" ");
		if (var->flags & CVAR_CHEAT)		Com_Printf("C");	else Com_Printf(" ");
		if (var->flags & CVAR_USER_CREATED)	Com_Printf("?");	else Com_Printf(" ");

		Com_Printf(S_COLOR_WHITE " %s = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE, var->name, var->string);
		if (var->latchedString)
			Com_Printf(", latched = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE, var->latchedString);
		Com_Printf("\n");
	}

	Com_Printf("\n%i total cvars\n", i);
	if (i != cvar_numIndexes)
		Com_Printf("%i cvar indexes\n", cvar_numIndexes);
}

void Cvar_SerenityJediEngine_f(void)
{
	Com_Printf("-----------------------------------------------------------------\n");
	Com_Printf("---------- Genuine SerenityJediEngine-(Solaris Edition)----------\n");
	Com_Printf("----------------------Build 02/07/2022---------------------------\n");
	Com_Printf("-----------------------------------------------------------------\n");
	Com_Printf("------------------------LightSaber-------------------------------\n");
	Com_Printf("-----------An elegant weapon for a more civilized age------------\n");
	Com_Printf("-----------------------------------------------------------------\n");
	Com_Printf("This mod improves the hit detection and alters the saber traces.\n");
	Com_Printf("Additionally,the first part of a swing will now do damage.\n");
	Com_Printf("This allows the beginning of the swing to be blocked/parried.\n");
	Com_Printf("Because of this new early-block system, saber spam or heavy\n");
	Com_Printf("exploits will be less effective.\n");
	Com_Printf("A player can 'deny' an enemy who wildly swings their saber\n");
	Com_Printf("and will force them to make cleaner, more precise hits.\n");
	Com_Printf("To make up for the beginning of the swing's shortcomings\n");
	Com_Printf("I added a 50 percent increase to end-phase damage.\n");
	Com_Printf("More damage at the end of the swing should maintain the fast-paced feel.\n");
	Com_Printf("Well-timed and well-aimed slashes are more effective than on basejka.\n");
	Com_Printf("This could potentially improve the way we play JKA quite a lot.\n");
	Com_Printf("Less spam-friendly, and more accurate hit detection could make the game more movie like.\n");
	Com_Printf("-----------------------------------------------------------------\n");
}

void Cvar_ListModified_f(void) {
	// build a list of cvars that are modified
	for (const cvar_t* var = cvar_vars;
		var;
		var = var->next)
	{
		char* value = var->latchedString ? var->latchedString : var->string;
		if (!var->name || !var->modificationCount || strcmp(value, var->resetString) == 0)
			continue;

		Com_Printf(S_COLOR_GREY "Cvar "
			S_COLOR_WHITE "%s = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE ", "
			S_COLOR_WHITE "default = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE "\n",
			var->name, value, var->resetString);
	}
}

void Cvar_ListUserCreated_f(void) {
	uint32_t count = 0;

	// build a list of cvars that are modified
	for (const cvar_t* var = cvar_vars;
		var;
		var = var->next)
	{
		char* value = var->latchedString ? var->latchedString : var->string;
		if (!(var->flags & CVAR_USER_CREATED))
			continue;

		Com_Printf(S_COLOR_GREY "Cvar "
			S_COLOR_WHITE "%s = " S_COLOR_GREY "\"" S_COLOR_WHITE "%s" S_COLOR_GREY "\"" S_COLOR_WHITE "\n",
			var->name, value);
		count++;
	}

	if (count > 0)
		Com_Printf(S_COLOR_GREY "Showing " S_COLOR_WHITE "%u" S_COLOR_GREY " user created cvars" S_COLOR_WHITE "\n", count);
	else
		Com_Printf(S_COLOR_GREY "No user created cvars" S_COLOR_WHITE "\n");
}

/*
============
Cvar_Unset

Unsets a cvar
============
*/

cvar_t* Cvar_Unset(cvar_t* cv)
{
	cvar_t* next = cv->next;

	// note what types of cvars have been modified (userinfo, archive, serverinfo, systeminfo)
	cvar_modifiedFlags |= cv->flags;

	if (cv->name)
		Cvar_FreeString(cv->name);
	if (cv->description)
		Cvar_FreeString(cv->description);
	if (cv->string)
		Cvar_FreeString(cv->string);
	if (cv->latchedString)
		Cvar_FreeString(cv->latchedString);
	if (cv->resetString)
		Cvar_FreeString(cv->resetString);

	if (cv->prev)
		cv->prev->next = cv->next;
	else
		cvar_vars = cv->next;
	if (cv->next)
		cv->next->prev = cv->prev;

	if (cv->hashPrev)
		cv->hashPrev->hashNext = cv->hashNext;
	else
		hashTable[cv->hashIndex] = cv->hashNext;
	if (cv->hashNext)
		cv->hashNext->hashPrev = cv->hashPrev;

	memset(cv, 0, sizeof * cv);

	return next;
}

/*
============
Cvar_Unset_f

Unsets a userdefined cvar
============
*/

void Cvar_Unset_f(void)
{
	if (Cmd_Argc() != 2)
	{
		Com_Printf("Usage: %s <varname>\n", Cmd_Argv(0));
		return;
	}

	cvar_t* cv = Cvar_FindVar(Cmd_Argv(1));

	if (!cv)
		return;

	if (cv->flags & CVAR_USER_CREATED)
		Cvar_Unset(cv);
	else
		Com_Printf("Error: %s: Variable %s is not user created.\n", Cmd_Argv(0), cv->name);
}

void Cvar_UnsetUserCreated_f(void)
{
	cvar_t* curvar = cvar_vars;
	uint32_t count = 0;

	while (curvar)
	{
		if (curvar->flags & CVAR_USER_CREATED)
		{
			// throw out any variables the user created
			curvar = Cvar_Unset(curvar);
			count++;
			continue;
		}
		curvar = curvar->next;
	}

	if (count > 0)
		Com_Printf(S_COLOR_GREY "Removed " S_COLOR_WHITE "%u" S_COLOR_GREY " user created cvars" S_COLOR_WHITE "\n", count);
	else
		Com_Printf(S_COLOR_GREY "No user created cvars to remove" S_COLOR_WHITE "\n");
}

/*
============
Cvar_Restart

Resets all cvars to their hardcoded values and removes userdefined variables
and variables added via the VMs if requested.
============
*/

void Cvar_Restart(qboolean unsetVM)
{
	cvar_t* curvar = cvar_vars;

	while (curvar)
	{
		if (curvar->flags & CVAR_USER_CREATED ||
			unsetVM && curvar->flags & CVAR_VM_CREATED)
		{
			// throw out any variables the user/vm created
			curvar = Cvar_Unset(curvar);
			continue;
		}

		if (!(curvar->flags & (CVAR_ROM | CVAR_INIT | CVAR_NORESTART)))
		{
			// Just reset the rest to their default values.
			Cvar_SetSafe(curvar->name, curvar->resetString);
		}

		curvar = curvar->next;
	}
}

/*
============
Cvar_Restart_f

Resets all cvars to their hardcoded values
============
*/
void Cvar_Restart_f(void) {
	Cvar_Restart(qfalse);
}

/*
=====================
Cvar_InfoString
=====================
*/
char* Cvar_InfoString(int bit) {
	static char	info[MAX_INFO_STRING];

	info[0] = 0;

	for (const cvar_t* var = cvar_vars; var; var = var->next)
	{
		if (!(var->flags & CVAR_INTERNAL) && var->name &&
			var->flags & bit)
		{
			Info_SetValueForKey(info, var->name, var->string);
		}
	}

	return info;
}

/*
=====================
Cvar_InfoString_Big

  handles large info strings ( CS_SYSTEMINFO )
=====================
*/
char* Cvar_InfoString_Big(int bit) {
	static char	info[BIG_INFO_STRING];

	info[0] = 0;

	for (const cvar_t* var = cvar_vars; var; var = var->next)
	{
		if (!(var->flags & CVAR_INTERNAL) && var->name &&
			var->flags & bit)
		{
			Info_SetValueForKey_Big(info, var->name, var->string);
		}
	}
	return info;
}

/*
=====================
Cvar_InfoStringBuffer
=====================
*/
void Cvar_InfoStringBuffer(int bit, char* buff, int buffsize) {
	Q_strncpyz(buff, Cvar_InfoString(bit), buffsize);
}

/*
=====================
Cvar_CheckRange
=====================
*/
void Cvar_CheckRange(cvar_t* var, float min, float max, qboolean integral)
{
	var->validate = qtrue;
	var->min = min;
	var->max = max;
	var->integral = integral;

	// Force an initial range check
	Cvar_Set(var->name, var->string);
}

/*
=====================
Cvar_Register

basically a slightly modified Cvar_Get for the interpreted modules
=====================
*/
void	Cvar_Register(vmCvar_t* vmCvar, const char* varName, const char* defaultValue, uint32_t flags) {
	// There is code in Cvar_Get to prevent CVAR_ROM cvars being changed by the user. In other words CVAR_ARCHIVE and
	//	CVAR_ROM are mutually exclusive flags. Unfortunately some historical game code (including single player baseq3)
	//	sets both flags. We unset CVAR_ROM for such cvars.
	if ((flags & (CVAR_ARCHIVE | CVAR_ROM)) == (CVAR_ARCHIVE | CVAR_ROM)) {
		Com_DPrintf(S_COLOR_YELLOW "WARNING: Unsetting CVAR_ROM cvar '%s', since it is also CVAR_ARCHIVE\n", varName);
		flags &= ~CVAR_ROM;
	}

	const cvar_t* cv = Cvar_Get(varName, defaultValue, flags | CVAR_VM_CREATED);
	if (!vmCvar) {
		return;
	}
	vmCvar->handle = cv - cvar_indexes;
	vmCvar->modificationCount = -1;
	Cvar_Update(vmCvar);
}

/*
=====================
Cvar_Update

updates an interpreted modules' version of a cvar
=====================
*/
void	Cvar_Update(vmCvar_t* vmCvar) {
	assert(vmCvar);

	if (static_cast<unsigned>(vmCvar->handle) >= static_cast<unsigned>(cvar_numIndexes)) {
		Com_Error(ERR_DROP, "Cvar_Update: handle %u out of range", static_cast<unsigned>(vmCvar->handle));
	}

	const cvar_t* cv = cvar_indexes + vmCvar->handle;

	if (cv->modificationCount == vmCvar->modificationCount) {
		return;
	}
	if (!cv->string) {
		return;		// variable might have been cleared by a cvar_restart
	}
	vmCvar->modificationCount = cv->modificationCount;
	if (strlen(cv->string) + 1 > MAX_CVAR_VALUE_STRING)
		Com_Error(ERR_DROP, "Cvar_Update: src %s length %u exceeds MAX_CVAR_VALUE_STRING", cv->string, (unsigned int)strlen(cv->string));
	Q_strncpyz(vmCvar->string, cv->string, MAX_CVAR_VALUE_STRING);

	vmCvar->value = cv->value;
	vmCvar->integer = cv->integer;
}

/*
==================
Cvar_CompleteCvarName
==================
*/
void Cvar_CompleteCvarName(char* args, int argNum)
{
	if (argNum == 2)
	{
		// Skip "<cmd> "
		char* p = Com_SkipTokens(args, 1, " ");

		if (p > args)
			Field_CompleteCommand(p, qfalse, qtrue);
	}
}

/*
============
Cvar_Init

Reads in all archived cvars
============
*/
void Cvar_Init(void) {
	memset(cvar_indexes, 0, sizeof cvar_indexes);
	memset(hashTable, 0, sizeof hashTable);

	cvar_cheats = Cvar_Get("sv_cheats", "1", CVAR_ROM | CVAR_SYSTEMINFO, "Allow cheats on server if set to 1");

	r_weather = Cvar_Get("r_weather", "0", CVAR_ARCHIVE, "");

	Cmd_AddCommand("print", Cvar_Print_f, "Print cvar help");
	Cmd_SetCommandCompletionFunc("print", Cvar_CompleteCvarName);
	Cmd_AddCommand("toggle", Cvar_Toggle_f, "Toggle a cvar between values");
	Cmd_SetCommandCompletionFunc("toggle", Cvar_CompleteCvarName);
	Cmd_AddCommand("set", Cvar_Set_f, "Set a cvar");
	Cmd_SetCommandCompletionFunc("set", Cvar_CompleteCvarName);
	Cmd_AddCommand("sets", Cvar_Set_f, "Set a cvar and apply serverinfo flag");
	Cmd_SetCommandCompletionFunc("sets", Cvar_CompleteCvarName);
	Cmd_AddCommand("setu", Cvar_Set_f, "Set a cvar and apply userinfo flag");
	Cmd_SetCommandCompletionFunc("setu", Cvar_CompleteCvarName);
	Cmd_AddCommand("seta", Cvar_Set_f, "Set a cvar and apply archive flag");
	Cmd_SetCommandCompletionFunc("seta", Cvar_CompleteCvarName);
	Cmd_AddCommand("cvarAdd", Cvar_Math_f, "Add a value to a cvar");
	Cmd_SetCommandCompletionFunc("cvarAdd", Cvar_CompleteCvarName);
	Cmd_AddCommand("cvarSub", Cvar_Math_f, "Subtract a value from a cvar");
	Cmd_SetCommandCompletionFunc("cvarSub", Cvar_CompleteCvarName);
	Cmd_AddCommand("cvarMult", Cvar_Math_f, "Multiply a value to a cvar");
	Cmd_SetCommandCompletionFunc("cvarMult", Cvar_CompleteCvarName);
	Cmd_AddCommand("cvarDiv", Cvar_Math_f, "Divide a value from a cvar");
	Cmd_SetCommandCompletionFunc("cvarDiv", Cvar_CompleteCvarName);
	Cmd_AddCommand("cvarMod", Cvar_Math_f, "Apply a modulo on a cvar");
	Cmd_SetCommandCompletionFunc("cvarMod", Cvar_CompleteCvarName);
	Cmd_AddCommand("reset", Cvar_Reset_f, "Reset a cvar to default");
	Cmd_SetCommandCompletionFunc("reset", Cvar_CompleteCvarName);
	Cmd_AddCommand("unset", Cvar_Unset_f, "Unset a user generated cvar");
	Cmd_SetCommandCompletionFunc("unset", Cvar_CompleteCvarName);
	Cmd_AddCommand("unset_usercreated", Cvar_UnsetUserCreated_f, "Unset all user generated cvars " S_COLOR_RED "Use with caution!" S_COLOR_WHITE);
	Cmd_AddCommand("cvarlist", Cvar_List_f, "Show all cvars");
	Cmd_AddCommand("cvar_usercreated", Cvar_ListUserCreated_f, "Show all user created cvars");
	Cmd_AddCommand("cvar_modified", Cvar_ListModified_f, "Show all modified cvars");
	Cmd_AddCommand("cvar_restart", Cvar_Restart_f, "Resetart the cvar sub-system");
	Cmd_AddCommand("serenityjediengine", Cvar_SerenityJediEngine_f, "serenityjediengine note");
}

static void Cvar_Realloc(char** string, char* memPool, int& memPoolUsed)
{
	if (string && *string)
	{
		char* temp = memPool + memPoolUsed;
		strcpy(temp, *string);
		memPoolUsed += strlen(*string) + 1;
		Cvar_FreeString(*string);
		*string = temp;
	}
}

//Turns many small allocation blocks into one big one.
void Cvar_Defrag(void)
{
	cvar_t* var;
	int totalMem = 0;

	for (var = cvar_vars; var; var = var->next)
	{
		if (var->name) {
			totalMem += strlen(var->name) + 1;
		}
		if (var->description) {
			totalMem += strlen(var->description) + 1;
		}
		if (var->string) {
			totalMem += strlen(var->string) + 1;
		}
		if (var->resetString) {
			totalMem += strlen(var->resetString) + 1;
		}
		if (var->latchedString) {
			totalMem += strlen(var->latchedString) + 1;
		}
	}

	char* mem = static_cast<char*>(Z_Malloc(totalMem, TAG_SMALL, qfalse));
	const int nextMemPoolSize = totalMem;
	totalMem = 0;

	for (var = cvar_vars; var; var = var->next)
	{
		Cvar_Realloc(&var->name, mem, totalMem);
		Cvar_Realloc(&var->string, mem, totalMem);
		Cvar_Realloc(&var->resetString, mem, totalMem);
		Cvar_Realloc(&var->latchedString, mem, totalMem);
		Cvar_Realloc(&var->description, mem, totalMem);
	}

	if (lastMemPool) {
		Z_Free(lastMemPool);
	}
	lastMemPool = mem;
	memPoolSize = nextMemPoolSize;
}