/*
===========================================================================
Copyright (C) 2000 - 2013, Raven Software, Inc.
Copyright (C) 2001 - 2013, Activision, Inc.
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

/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// ///
///																																///
///																																///
///													SERENITY JEDI ENGINE														///
///										          LIGHTSABER COMBAT SYSTEM													    ///
///																																///
///						      System designed by Serenity and modded by JaceSolaris. (c) 2019 SJE   		                    ///
///								    https://www.moddb.com/mods/serenityjediengine-20											///
///																																///
/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// ///

#include "g_local.h"
#include "b_local.h"
#include "wp_saber.h"
#include "g_vehicles.h"
#include "../qcommon/tri_coll_test.h"
#include "../cgame/cg_local.h"

//////////Defines////////////////
extern qboolean PM_KickingAnim(int anim);
extern qboolean BG_SaberInNonIdleDamageMove(const playerState_t* ps);
extern qboolean in_front(vec3_t spot, vec3_t from, vec3_t from_angles, float thresh_hold = 0.0f);
extern qboolean PM_SaberInKnockaway(int move);
extern qboolean PM_SaberInBounce(int move);
extern qboolean BG_InSlowBounce(const playerState_t* ps);
extern qboolean G_ControlledByPlayer(gentity_t* self);
extern qboolean PM_InKnockDown(const playerState_t* ps);
extern qboolean PM_SaberInTransitionAny(int move);
extern void G_AddVoiceEvent(gentity_t* self, int event, int speakDebounceTime);
extern qboolean npc_is_dark_jedi(const gentity_t* self);
extern saberMoveName_t pm_broken_parry_for_attack(int move);
extern qboolean PM_InGetUp(const playerState_t* ps);
extern qboolean PM_InForceGetUp(const playerState_t* ps);
extern qboolean PM_SaberInParry(int move);
extern saberMoveName_t PM_AbsorbtheParry(int move);
extern saberMoveName_t PM_KnockawayForParry(int move);
extern int G_KnockawayForParry(int move);
extern int g_absorbthe_parry(int move);
extern qboolean PM_VelocityForBlockedMove(const playerState_t* ps, vec3_t throwDir);
extern void PM_VelocityForSaberMove(const playerState_t* ps, vec3_t throw_dir);
extern qboolean WP_SaberLose(gentity_t* self, vec3_t throwDir);
extern cvar_t* g_saberAutoBlocking;
extern qboolean WP_SabersCheckLock(gentity_t* ent1, gentity_t* ent2);
extern qboolean walk_check(const gentity_t* self);
extern void G_Knockdown(gentity_t* self, gentity_t* attacker, const vec3_t pushDir, float strength,
	qboolean breakSaberLock);
extern qboolean PM_SuperBreakWinAnim(int anim);
extern void PM_AddFatigue(playerState_t* ps, int Fatigue);
extern qboolean WP_SaberBlockNonRandom(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
extern saberMoveName_t PM_BrokenParryForParry(int move);
extern saberMoveName_t PM_MBlocktheAttack(int move);
extern int G_MBlocktheAttack(int move);
extern qboolean WP_BrokenBoltBlockKnockBack(gentity_t* victim, gentity_t* attacker, qboolean allowAnyMove);
extern void PM_AddBlockFatigue(playerState_t* ps, int Fatigue);
extern void G_Stagger(gentity_t* hit_ent, gentity_t* atk, qboolean allowAnyMove);
extern void G_StaggerAttacker(gentity_t* atk, gentity_t* hitEnt, qboolean allowAnyMove);
extern void wp_block_points_regenerate(const gentity_t* self, int override_amt);
extern saberMoveName_t PM_SaberBounceForAttack(int move);
extern saberMoveName_t PM_SaberAttackStagger(int move);
extern cvar_t* g_SaberPerfectBlockingTimer;
extern cvar_t* g_SaberPerfectBlockingwaitTimer;
extern void WP_SaberDrop(const gentity_t* self, gentity_t* saber);
extern qboolean WP_SaberMBlockDirection(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
extern qboolean PM_SaberInnonblockableAttack(int anim);
extern qboolean pm_saber_in_special_attack(int anim);
extern qboolean PM_SaberInKata(saberMoveName_t saberMove);
extern void wp_saber_clear_damage_for_ent_num(gentity_t* attacker, int entity_num, int saber_num, int blade_num);
extern cvar_t* d_slowmoaction;
extern void G_StartStasisEffect(const gentity_t* ent, int meFlags = 0, int length = 1000, float timeScale = 0.0f,
	int spinTime = 0);
extern void CGCam_BlockShakeSP(float intensity, int duration);
extern int G_GetParryForBlock(int block);
extern qboolean WP_SaberDisarmed(gentity_t* self, vec3_t throwDir);
extern void wp_block_points_regenerate_over_ride(const gentity_t* self, int override_amt);
extern void G_SaberAttackBounce(gentity_t* self, gentity_t* other, qboolean hitBody);
extern qboolean WP_SaberBouncedSaberDirection(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
// Saber Blocks
extern qboolean WP_SaberMBlock(gentity_t* victim, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberBlockedBounceBlock(gentity_t* victim, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberFatiguedParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberNPCParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberNPCMBlock(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberNPCFatiguedParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
void SabBeh_AnimateHeavySlowBounceAttacker(gentity_t* attacker, gentity_t* Blocker);
void SabBeh_AnimateHeavySlowBounceBlocker(gentity_t* blocker, gentity_t* attacker);
extern cvar_t* g_DebugSaberCombat;
///////////Defines////////////////

//////////Actions////////////////
qboolean G_TransitionParry(const gentity_t* self)
{
	//checks to see if a player is doing an attack parry
	if (self->client->pers.cmd.buttons & BUTTON_ATTACK
		|| self->client->pers.cmd.buttons & BUTTON_ALT_ATTACK)
	{
		//can't be pressing an attack button.
		return qfalse;
	}

	if (self->client->ps.userInt3 & 1 << FLAG_PARRIED)
	{
		//can't attack parry when parried.
		return qfalse;
	}

	if (PM_SaberInTransitionAny(self->client->ps.saberMove))
	{
		//in transition, start, or return
		return qtrue;
	}

	return qfalse;
}

void SabBeh_SaberShouldBeDisarmedAttacker(gentity_t* attacker, gentity_t* blocker, int saberNum, int bladeNum)
{
	if (saberNum == 0)
	{
		//can only lose right-hand saber for now
		if (!(attacker->client->ps.saber[saberNum].saberFlags & SFL_NOT_DISARMABLE))
		{
			//knocked the saber right out of his hand!
			vec3_t throwDir = { 0, 0, 350 };

			WP_SaberDisarmed(attacker, throwDir);
		}
	}
}

void SabBeh_SaberShouldBeDisarmedBlocker(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum)
{
	if (saberNum == 0)
	{
		//can only lose right-hand saber for now
		if (!(blocker->client->ps.saber[saberNum].saberFlags & SFL_NOT_DISARMABLE))
		{
			//knocked the saber right out of his hand!
			vec3_t throwDir = { 0, 0, 350 };

			WP_SaberDisarmed(blocker, throwDir);
		}
	}
}

qboolean g_accurate_blocking(const gentity_t* self, const gentity_t* attacker, vec3_t hit_loc)
{
	//determines if self (who is blocking) is actively blocking (parrying)
	vec3_t p_angles;
	vec3_t p_right;
	vec3_t parrier_move;
	vec3_t hit_pos;
	vec3_t hit_flat; //flatten 2D version of the hitPos.
	const qboolean in_front_of_me = in_front(attacker->client->ps.origin, self->client->ps.origin,
		self->client->ps.viewangles, 0.0f);

	if (self->s.number < MAX_CLIENTS)
	{
		if (!(self->client->ps.ManualBlockingFlags & 1 << MBF_BLOCKING))
		{
			return qfalse;
		}
	}

	if (!in_front_of_me)
	{
		//can't parry attacks to the rear.
		return qfalse;
	}
	if (PM_SaberInKnockaway(self->client->ps.saberMove))
	{
		//already in parry move, continue parrying anything that hits us as long as
		//the attacker is in the same general area that we're facing.
		return qtrue;
	}

	if (PM_KickingAnim(self->client->ps.legsAnim))
	{
		//can't parry in kick.
		return qfalse;
	}

	if (BG_SaberInNonIdleDamageMove(&self->client->ps)
		|| PM_SaberInBounce(self->client->ps.saberMove) || BG_InSlowBounce(&self->client->ps))
	{
		//can't parry if we're transitioning into a block from an attack state.
		return qfalse;
	}

	if (self->client->ps.pm_flags & PMF_DUCKED)
	{
		//can't parry while ducked or running
		return qfalse;
	}

	if (PM_InKnockDown(&self->client->ps))
	{
		//can't block while knocked down or getting up from knockdown, or we are staggered.
		return qfalse;
	}

	//set up flatten version of the location of the incoming attack in orientation
	//to the player.
	VectorSubtract(hit_loc, self->client->ps.origin, hit_pos);
	VectorSet(p_angles, 0, self->client->ps.viewangles[YAW], 0);
	AngleVectors(p_angles, nullptr, p_right, nullptr);
	hit_flat[0] = 0;
	hit_flat[1] = DotProduct(p_right, hit_pos);

	//just bump the hit pos down for the blocking since the average left/right slice happens at about origin +10
	hit_flat[2] = hit_pos[2] - 10;
	VectorNormalize(hit_flat);

	//set up the vector for the direction the player is trying to parry in.
	parrier_move[0] = 0;
	parrier_move[1] = self->client->pers.cmd.rightmove;
	parrier_move[2] = -self->client->pers.cmd.forwardmove;
	VectorNormalize(parrier_move);

	const float block_dot = DotProduct(hit_flat, parrier_move);

	if (block_dot >= .4)
	{
		//player successfully blocked in the right direction to do a full parry.
		return qtrue;
	}
	//player didn't parry in the correct direction, do blockPoints punishment
	if (self->NPC)
	{
		//bots just randomly parry to make up for them not intelligently parrying.
		if (NPC_PARRYRATE * g_spskill->integer > Q_irand(0, 999))
		{
			return qtrue;
		}
	}
	return qfalse;
}

qboolean g_perfect_blocking(const gentity_t* self, const gentity_t* attacker, vec3_t hitLoc)
{
	//determines if self (who is blocking) is actively m blocking (parrying)
	vec3_t p_angles;
	vec3_t p_right;
	vec3_t parrier_move;
	vec3_t hit_pos;
	vec3_t hit_flat; //flatten 2D version of the hitPos.
	const qboolean in_front_of_me = in_front(attacker->client->ps.origin, self->client->ps.origin,
		self->client->ps.viewangles, 0.0f);

	if (self->s.number < MAX_CLIENTS)
	{
		if (!(self->client->ps.ManualBlockingFlags & 1 << MBF_PROJBLOCKING))
		{
			return qfalse;
		}
	}

	if (!in_front_of_me)
	{
		//can't parry attacks to the rear.
		return qfalse;
	}
	if (PM_SaberInKnockaway(self->client->ps.saberMove))
	{
		//already in parry move, continue parrying anything that hits us as long as
		//the attacker is in the same general area that we're facing.
		return qtrue;
	}

	if (self->client->ps.ManualblockLastStartTime >= g_SaberPerfectBlockingwaitTimer->integer) //3 sec
	{
		//cant perfect parry if your too slow
		return qfalse;
	}

	if (PM_KickingAnim(self->client->ps.legsAnim))
	{
		//can't parry in kick.
		return qfalse;
	}

	if (BG_SaberInNonIdleDamageMove(&self->client->ps)
		|| PM_SaberInBounce(self->client->ps.saberMove) || BG_InSlowBounce(&self->client->ps))
	{
		//can't parry if we're transitioning into a block from an attack state.
		return qfalse;
	}

	if (self->client->ps.pm_flags & PMF_DUCKED)
	{
		//can't parry while ducked or running
		return qfalse;
	}

	if (PM_InKnockDown(&self->client->ps))
	{
		//can't block while knocked down or getting up from knockdown, or we are staggered.
		return qfalse;
	}

	//set up flatten version of the location of the incoming attack in orientation
	//to the player.
	VectorSubtract(hitLoc, self->client->ps.origin, hit_pos);
	VectorSet(p_angles, 0, self->client->ps.viewangles[YAW], 0);
	AngleVectors(p_angles, nullptr, p_right, nullptr);
	hit_flat[0] = 0;
	hit_flat[1] = DotProduct(p_right, hit_pos);

	//just bump the hit pos down for the blocking since the average left/right slice happens at about origin +10
	hit_flat[2] = hit_pos[2] - 10;
	VectorNormalize(hit_flat);

	//set up the vector for the direction the player is trying to parry in.
	parrier_move[0] = 0;
	parrier_move[1] = self->client->pers.cmd.rightmove;
	parrier_move[2] = -self->client->pers.cmd.forwardmove;
	VectorNormalize(parrier_move);

	const float block_dot = DotProduct(hit_flat, parrier_move);

	if (block_dot >= .4)
	{
		//player successfully blocked in the right direction
		return qtrue;
	}
	//player didn't parry in the correct direction, do blockPoints punishment
	self->client->ps.blockPoints -= BLOCKPOINTS_FAIL;

	if (self->NPC)
	{
		//bots just randomly parry to make up for them not intelligently parrying.
		if (NPC_PARRYRATE * g_spskill->integer > Q_irand(0, 999))
		{
			return qtrue;
		}
	}
	return qfalse;
}

void SabBeh_AddMishap_Attacker(gentity_t* attacker, gentity_t* blocker, int saberNum, int bladeNum)
{
	if (attacker->client->ps.saberAttackChainCount <= MISHAPLEVEL_NONE)
	{
		attacker->client->ps.saberAttackChainCount = MISHAPLEVEL_NONE;
	}
	else
	{
		//overflowing causes a full mishap.
		const int randNum = Q_irand(0, 2);

		switch (randNum)
		{
		case 0:
			SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
			if (d_attackinfo->integer || g_DebugSaberCombat->integer)
			{
				gi.Printf(S_COLOR_YELLOW"Attacker staggering\n");
			}
			break;
		case 1:
			SabBeh_SaberShouldBeDisarmedAttacker(attacker, blocker, saberNum, bladeNum);
			if (d_attackinfo->integer || g_DebugSaberCombat->integer)
			{
				gi.Printf(S_COLOR_RED"Attacker lost his saber\n");
			}
			break;
		default:;
		}
	}
}

void SabBeh_AddMishap_Blocker(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum)
{
	if (blocker->client->ps.blockPoints <= MISHAPLEVEL_NONE)
	{
		blocker->client->ps.blockPoints = MISHAPLEVEL_NONE;
	}
	else
	{
		//overflowing causes a full mishap.
		const int randNum = Q_irand(0, 2);

		switch (randNum)
		{
		case 0:
			G_Stagger(blocker, attacker, qtrue);
			if (d_combatinfo->integer || g_DebugSaberCombat->integer)
			{
				gi.Printf(S_COLOR_YELLOW"blocker staggering\n");
			}
			break;
		case 1:
			SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker, saberNum, bladeNum);
			if (d_combatinfo->integer || g_DebugSaberCombat->integer)
			{
				gi.Printf(S_COLOR_RED"blocker lost his saber\n");
			}
			wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
			break;
		default:;
		}
	}
}

////////Bounces//////////

void SabBeh_AnimateHeavySlowBounceAttacker(gentity_t* attacker, gentity_t* Blocker)
{
	G_StaggerAttacker(attacker, Blocker, qtrue);
}

void SabBeh_AnimateHeavySlowBounceBlocker(gentity_t* blocker, gentity_t* attacker)
{
	blocker->client->ps.userInt3 |= 1 << FLAG_SLOWBOUNCE;
	blocker->client->ps.userInt3 |= 1 << FLAG_OLDSLOWBOUNCE;

	G_AddEvent(blocker, Q_irand(EV_PUSHED1, EV_PUSHED3), 0);
	G_AddEvent(attacker, Q_irand(EV_DEFLECT1, EV_DEFLECT3), 0);

	blocker->client->ps.saberBounceMove = pm_broken_parry_for_attack(blocker->client->ps.saberMove);
	blocker->client->ps.saberBlocked = BLOCKED_PARRY_BROKEN;
}

void SabBeh_AnimateSmallBounce(const gentity_t* attacker, gentity_t* blocker)
{
	attacker->client->ps.userInt3 |= 1 << FLAG_SLOWBOUNCE;

	attacker->client->ps.saberBounceMove = PM_SaberBounceForAttack(attacker->client->ps.saberMove);
	attacker->client->ps.saberBlocked = BLOCKED_ATK_BOUNCE;
}

void SabBeh_AnimateSlowBounceBlocker(gentity_t* blocker, gentity_t* attacker)
{
	blocker->client->ps.userInt3 |= 1 << FLAG_SLOWBOUNCE;
	blocker->client->ps.userInt3 |= 1 << FLAG_OLDSLOWBOUNCE;

	G_AddEvent(blocker, Q_irand(EV_PUSHED1, EV_PUSHED3), 0);

	blocker->client->ps.saberBounceMove = PM_BrokenParryForParry(G_GetParryForBlock(blocker->client->ps.saberBlocked));
	blocker->client->ps.saberBlocked = BLOCKED_PARRY_BROKEN;
}

////////Bounces//////////

qboolean SabBeh_RollBalance_Advanced(gentity_t* attacker, gentity_t* blocker, int saberNum, int bladeNum,
	qboolean forceMishap)
{
	//JaceSolaris making it feel like EoC MP/OJP
	//if the attack is blocked -(Im the attacker)

	if (attacker->client->ps.saberAttackChainCount >= MISHAPLEVEL_FULL)
	{
		//hard mishap.
		if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
		{
			SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
			if ((d_attackinfo->integer || g_DebugSaberCombat->integer) && attacker->s.number < MAX_CLIENTS ||
				G_ControlledByPlayer(attacker))
			{
				gi.Printf(S_COLOR_GREEN"Attacker Advanced npc attacker is fatigued\n");
			}
		}
		else
		{
			SabBeh_AddMishap_Attacker(attacker, blocker, saberNum, bladeNum);
		}
		attacker->client->ps.saberAttackChainCount = MISHAPLEVEL_LIGHT;
		return qtrue;
	}
	if (attacker->client->ps.saberAttackChainCount >= MISHAPLEVEL_HUDFLASH)
	{
		//slow bounce
		SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
		attacker->client->ps.saberAttackChainCount = MISHAPLEVEL_LIGHT;
		if ((d_attackinfo->integer || g_DebugSaberCombat->integer) && attacker->s.number < MAX_CLIENTS ||
			G_ControlledByPlayer(attacker))
		{
			gi.Printf(S_COLOR_GREEN"Attacker Advanced stagger\n");
		}
		return qtrue;
	}
	if (attacker->client->ps.saberAttackChainCount >= MISHAPLEVEL_LIGHT)
	{
		//slow bounce
		if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
		{
			G_SaberAttackBounce(attacker, blocker, qfalse);
		}
		else
		{
			SabBeh_AnimateSmallBounce(attacker, blocker);
		}
		if ((d_attackinfo->integer || g_DebugSaberCombat->integer) && attacker->s.number < MAX_CLIENTS ||
			G_ControlledByPlayer(attacker))
		{
			gi.Printf(S_COLOR_GREEN"Attacker Advanced small bounce\n");
		}
		return qtrue;
	}
	if (forceMishap)
	{
		//perform a slow bounce even if we don't have enough mishap for it.
		if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
		{
			G_SaberAttackBounce(attacker, blocker, qfalse);
		}
		else
		{
			SabBeh_AnimateSmallBounce(attacker, blocker);
		}
		if ((d_attackinfo->integer || g_DebugSaberCombat->integer) && attacker->s.number < MAX_CLIENTS ||
			G_ControlledByPlayer(attacker))
		{
			gi.Printf(S_COLOR_GREEN"Attacker Advanced blocked bounce\n");
		}
		return qtrue;
	}
	return qfalse;
}

void SabBeh_AddBalance(const gentity_t* self, int amount)
{
	if (!walk_check(self))
	{
		//running or moving very fast, can't balance as well
		if (amount > 0)
		{
			amount *= 2;
		}
		else
		{
			amount = amount * .5f;
		}
	}

	self->client->ps.saberAttackChainCount += amount;

	if (self->client->ps.saberAttackChainCount < MISHAPLEVEL_NONE)
	{
		self->client->ps.saberAttackChainCount = MISHAPLEVEL_NONE;
	}
	else if (self->client->ps.saberAttackChainCount > MISHAPLEVEL_OVERLOAD)
	{
		self->client->ps.saberAttackChainCount = MISHAPLEVEL_MAX;
	}
}

//////////Actions////////////////

/////////Functions//////////////

void SabBeh_AttackVsAttack(gentity_t* attacker, gentity_t* blocker, int saberNum, int bladeNum)
{
	//set the saber behavior for two attacking blades hitting each other
	const qboolean atkfake = attacker->client->ps.userInt3 & 1 << FLAG_ATTACKFAKE ? qtrue : qfalse;
	const qboolean otherfake = blocker->client->ps.userInt3 & 1 << FLAG_ATTACKFAKE ? qtrue : qfalse;

	if (atkfake && !otherfake)
	{
		//self is solo faking
		//set self
		SabBeh_AddBalance(attacker, 1);
		//set otherOwner

		if (WP_SabersCheckLock(attacker, blocker))
		{
			attacker->client->ps.userInt3 |= 1 << FLAG_LOCKWINNER;
			attacker->client->ps.saberBlocked = BLOCKED_NONE;
			blocker->client->ps.saberBlocked = BLOCKED_NONE;
		}
		SabBeh_AddBalance(blocker, -1);
	}
	else if (!atkfake && otherfake)
	{
		//only otherOwner is faking
		//set self
		if (WP_SabersCheckLock(blocker, attacker))
		{
			attacker->client->ps.saberBlocked = BLOCKED_NONE;
			blocker->client->ps.userInt3 |= 1 << FLAG_LOCKWINNER;
			blocker->client->ps.saberBlocked = BLOCKED_NONE;
		}
		SabBeh_AddBalance(attacker, -1);
		//set otherOwner
		SabBeh_AddBalance(blocker, 1);
	}
	else if (PM_SaberInKata(static_cast<saberMoveName_t>(attacker->client->ps.saberMove)))
	{
		SabBeh_AddBalance(attacker, 1);
		//set otherOwner
		SabBeh_AddBalance(blocker, -1);

		if (blocker->client->ps.blockPoints < BLOCKPOINTS_TEN)
		{
			//Low points = bad blocks
			SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker, saberNum, bladeNum);
			wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
		}
		else
		{
			//Low points = bad blocks
			G_Stagger(blocker, attacker, qtrue);
			PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
		}
	}
	else if (PM_SaberInKata(static_cast<saberMoveName_t>(blocker->client->ps.saberMove)))
	{
		SabBeh_AddBalance(attacker, -1);
		//set otherOwner
		SabBeh_AddBalance(blocker, 1);

		if (attacker->client->ps.blockPoints < BLOCKPOINTS_TEN)
		{
			//Low points = bad blocks
			SabBeh_SaberShouldBeDisarmedAttacker(attacker, blocker, saberNum, bladeNum);
			wp_block_points_regenerate_over_ride(attacker, BLOCKPOINTS_FATIGUE);
		}
		else
		{
			//Low points = bad blocks
			G_Stagger(attacker, blocker, qtrue);
			PM_AddBlockFatigue(&attacker->client->ps, BLOCKPOINTS_TEN);
		}
	}
	else
	{
		//either both are faking or neither is faking.  Either way, it's canceled out
		//set self
		SabBeh_AddBalance(attacker, 1);
		//set otherOwner
		SabBeh_AddBalance(blocker, 1);

		SabBeh_RollBalance_Advanced(attacker, blocker, saberNum, bladeNum, qtrue);

		SabBeh_RollBalance_Advanced(blocker, attacker, saberNum, bladeNum, qtrue);
	}
}

void SabBeh_AttackvBlock(gentity_t* attacker, gentity_t* blocker, int saberNum, int bladeNum)
{
	//if the attack is blocked -(Im the attacker)
	vec3_t hitLoc = { 0, 0, 1.0 };
	const qboolean perfectparry = g_perfect_blocking(blocker, attacker, hitLoc); //perfect Attack Blocking
	const qboolean AccurateParry = g_accurate_blocking(blocker, attacker, hitLoc); // Perfect Normal Blocking

	const qboolean Blocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_BLOCKING ? qtrue : qfalse;
	//Normal Blocking (just holding block button)
	const qboolean MBlocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_MBLOCKING ? qtrue : qfalse;
	//perfect Blocking (Timed Block)
	const qboolean ActiveBlocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_PROJBLOCKING ? qtrue : qfalse;
	//Active Blocking (Holding Block button = Attack button)

	const qboolean NPCBlocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_NPCBLOCKING ? qtrue : qfalse;
	//(Npc Blocking function)

	qboolean TransitionClashParry = G_TransitionParry(blocker);
	const qboolean atkfake = attacker->client->ps.userInt3 & 1 << FLAG_ATTACKFAKE ? qtrue : qfalse;

	if ((AccurateParry || perfectparry) && blocker->NPC && !G_ControlledByPlayer(blocker)
		&& NPC_PARRYRATE * g_spskill->integer > Q_irand(0, 999))
	{
		//npc performed an attack parry (by cheating a bit)
		TransitionClashParry = qtrue;
	}

	if (!PM_SaberInnonblockableAttack(attacker->client->ps.torsoAnim))
	{
		if (BG_SaberInNonIdleDamageMove(&blocker->client->ps) || TransitionClashParry)
			//Blocker is not blocking, he is attacking also
		{
			SabBeh_AttackVsAttack(attacker, blocker, saberNum, bladeNum);
			if (d_combatinfo->integer || g_DebugSaberCombat->integer)
			{
				gi.Printf(S_COLOR_YELLOW"Attacker Attack vs Attack\n");
			}
		}
		else if (PM_SuperBreakWinAnim(attacker->client->ps.torsoAnim))
		{
			//attacker was attempting a superbreak and he hit someone who could block the move, rail him for screwing up.
			SabBeh_AddBalance(attacker, 1);

			SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);

			SabBeh_AddBalance(blocker, -1);
			if (d_combatinfo->integer || g_DebugSaberCombat->integer)
			{
				gi.Printf(S_COLOR_YELLOW"Attacker Super break win / fail\n");
			}
		}
		else if (atkfake)
		{
			//attacker faked before making this attack, treat like standard attack/attack
			if (AccurateParry || perfectparry || Blocking || MBlocking || ActiveBlocking || NPCBlocking)
			{
				//defender parried the attack fake.
				SabBeh_AddBalance(attacker, MPCOST_PARRIED_ATTACKFAKE);

				SabBeh_RollBalance_Advanced(attacker, blocker, saberNum, bladeNum, qtrue);

				if (Blocking || MBlocking || ActiveBlocking || NPCBlocking)
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_QUICKPARRY;
				}
				else
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_PARRIED;
				}

				SabBeh_AddBalance(blocker, MPCOST_PARRYING_ATTACKFAKE);
				if (d_combatinfo->integer || g_DebugSaberCombat->integer)
				{
					gi.Printf(S_COLOR_YELLOW"Attacker Attack Fake Blocked\n");
				}
			}
			else
			{
				//otherwise, the defender stands a good chance of having his defensive broken.
				SabBeh_AddBalance(attacker, -1);

				if (attacker->client->ps.saberAnimLevel == SS_DESANN)
				{
					SabBeh_AddBalance(blocker, 2);
				}

				SabBeh_AnimateHeavySlowBounceBlocker(blocker, attacker);

				if (WP_SabersCheckLock(attacker, blocker))
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_LOCKWINNER;
					attacker->client->ps.saberBlocked = BLOCKED_NONE;
					blocker->client->ps.saberBlocked = BLOCKED_NONE;
				}

				G_SaberAttackBounce(attacker, blocker, qfalse);
				if (d_combatinfo->integer || g_DebugSaberCombat->integer)
				{
					gi.Printf(S_COLOR_YELLOW"Attacker Attack Fake bounced\n");
				}
			}
		}
		else
		{
			//standard attack
			if (Blocking || MBlocking || NPCBlocking || ActiveBlocking)
			{
				SabBeh_AddBalance(attacker, MPCOST_PARRIED);

				if (MBlocking || NPCBlocking)
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_QUICKPARRY;
				}
				else
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_PARRIED;
				}

				SabBeh_AddBalance(blocker, MPCOST_PARRYING);

				if ((Blocking || MBlocking || NPCBlocking) && attacker->client->ps.blockPoints <= BLOCKPOINTS_TEN)
				{
					SabBeh_AddMishap_Attacker(attacker, blocker, saberNum, bladeNum);
					if (d_combatinfo->integer || g_DebugSaberCombat->integer)
					{
						gi.Printf(S_COLOR_YELLOW"ATTACKER MISHAP WITH LOW BLOCK POINTS\n");
					}
					wp_block_points_regenerate(attacker, BLOCKPOINTS_FATIGUE);
				}
				else if ((Blocking || NPCBlocking) && !MBlocking && !ActiveBlocking)
				{
					SabBeh_RollBalance_Advanced(attacker, blocker, saberNum, bladeNum, qtrue);

					if (d_combatinfo->integer || g_DebugSaberCombat->integer)
					{
						if (NPCBlocking)
						{
							gi.Printf(S_COLOR_YELLOW"Attackers Blocker npc is using Block button spam\n");
						}
						else
						{
							gi.Printf(S_COLOR_YELLOW"Attackers Blocker is using Block button spam\n");
						}
					}
				}
				else
				{
					SabBeh_RollBalance_Advanced(attacker, blocker, saberNum, bladeNum, qtrue);

					if (d_combatinfo->integer || g_DebugSaberCombat->integer)
					{
						gi.Printf(S_COLOR_YELLOW"Attacker Most attacks handled here\n");
					}
				}
			}
			else
			{
				//Backup in case i missed some
				SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);

				SabBeh_AnimateHeavySlowBounceBlocker(blocker, attacker);

				PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);

				if (d_combatinfo->integer || g_DebugSaberCombat->integer)
				{
					gi.Printf(S_COLOR_MAGENTA"Attacker All the rest of the types of contact\n");
				}
			}
		}
	}
	else
	{
		//This must be Unblockable
		if (blocker->client->ps.blockPoints < BLOCKPOINTS_TEN)
		{
			//Low points = bad blocks
			SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker, saberNum, bladeNum);
			wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
		}
		else
		{
			//Low points = bad blocks
			G_Stagger(blocker, attacker, qtrue);
			PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
		}
		if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
			G_ControlledByPlayer(blocker))
		{
			gi.Printf(S_COLOR_YELLOW"Attacker must be Unblockable\n");
		}
	}
}

void SabBeh_BlockvsAttack(gentity_t* blocker, gentity_t* attacker, vec3_t hitLoc, int saberNum, int bladeNum)
{
	//-(Im the blocker)
	const qboolean perfectparry = g_perfect_blocking(blocker, attacker, hitLoc); //perfect Attack Blocking
	const qboolean AccurateParry = g_accurate_blocking(blocker, attacker, hitLoc); // Perfect Normal Blocking

	const qboolean Blocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_BLOCKING ? qtrue : qfalse;
	//Normal Blocking
	const qboolean MBlocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_MBLOCKING ? qtrue : qfalse;
	//perfect Blocking
	const qboolean ActiveBlocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_PROJBLOCKING ? qtrue : qfalse;
	//Active Blocking

	const qboolean NPCBlocking = blocker->client->ps.ManualBlockingFlags & 1 << MBF_NPCBLOCKING ? qtrue : qfalse;
	//Active NPC Blocking
	qboolean TransitionClashParry = G_TransitionParry(blocker);

	if ((AccurateParry || perfectparry) && (blocker->NPC && !G_ControlledByPlayer(blocker))
		&& NPC_PARRYRATE * g_spskill->integer > Q_irand(0, 999))
	{
		//npc performed an attack parry (by cheating a bit)
		TransitionClashParry = qtrue;
	}

	if (!PM_SaberInnonblockableAttack(attacker->client->ps.torsoAnim))
	{
		if (blocker->client->ps.blockPoints <= BLOCKPOINTS_FATIGUE)
		{
			if (blocker->client->ps.blockPoints <= BLOCKPOINTS_TEN)
			{
				//Low points = bad blocks
				if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
				{
					SabBeh_AddMishap_Blocker(blocker, attacker, saberNum, bladeNum);
				}
				else
				{
					SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker, saberNum, bladeNum);
					wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
				}

				if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
				{
					wp_block_points_regenerate(attacker, BLOCKPOINTS_FATIGUE);
				}
				if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
					G_ControlledByPlayer(blocker))
				{
					gi.Printf(S_COLOR_RED"Blocker Drop saber recharge to 20bp\n");
				}

				blocker->client->ps.saberEventFlags |= SEF_PARRIED;
				wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);
			}
			else
			{
				//Low points = bad blocks
				G_Stagger(blocker, attacker, qtrue);

				PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_DANGER);

				if (d_blockinfo->integer || g_DebugSaberCombat->integer && (blocker->s.number < MAX_CLIENTS || G_ControlledByPlayer(blocker)))
				{
					gi.Printf(S_COLOR_RED"Blocker stagger drain 4 bp\n");
				}

				blocker->client->ps.saberEventFlags |= SEF_PARRIED;
				wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);
			}
		}
		else if (blocker->client->ps.blockPoints <= BLOCKPOINTS_HALF)
		{
			if (blocker->client->ps.saberAnimLevel == SS_FAST ||
				blocker->client->ps.saberAnimLevel == SS_MEDIUM ||
				blocker->client->ps.saberAnimLevel == SS_STRONG ||
				blocker->client->ps.saberAnimLevel == SS_DESANN ||
				blocker->client->ps.saberAnimLevel == SS_TAVION ||
				blocker->client->ps.saberAnimLevel == SS_STAFF ||
				blocker->client->ps.saberAnimLevel == SS_DUAL)
			{
				if (ActiveBlocking) //Holding Block Button + attack button
				{
					if (MBlocking) // A perfectly timed block
					{
						WP_SaberMBlock(blocker, attacker, saberNum, bladeNum);

						blocker->client->ps.saberEventFlags |= SEF_PARRIED;
						wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

						wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);

						PM_AddBlockFatigue(&attacker->client->ps, BLOCKPOINTS_TEN);

						blocker->client->ps.saberAttackChainCount = MISHAPLEVEL_NONE;

						if (d_slowmoaction->integer && (blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker)))
						{
							G_StartStasisEffect(blocker, MEF_NO_SPIN, 200, 0.3f, 0);
							CGCam_BlockShakeSP(0.45f, 100);
						}
						G_Sound(blocker,
							G_SoundIndex(va("sound/weapons/saber/saber_perfectblock%d.mp3", Q_irand(1, 3))));
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Perfect block reward 20\n");
						}
						if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
						{
							SabBeh_AddMishap_Attacker(attacker, blocker, saberNum, bladeNum);
						}
					}
					else
					{
						//Spamming block + attack buttons
						WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);

						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_DANGER);

						if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
						{
							PM_AddBlockFatigue(&attacker->client->ps, BLOCKPOINTS_AUTO);
						}

						if (d_slowmoaction->integer && (blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker)))
						{
							CGCam_BlockShakeSP(0.45f, 100);
						}

						blocker->client->ps.saberEventFlags |= SEF_PARRIED;
						wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker spamming block + attack cost 4\n");
						}
					}
				}
				else if (Blocking && !ActiveBlocking) //Holding block button only (spamming block)
				{
					WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);

					if (d_slowmoaction->integer && (blocker->s.number < MAX_CLIENTS || G_ControlledByPlayer(blocker)))
					{
						CGCam_BlockShakeSP(0.45f, 100);
					}

					blocker->client->ps.saberEventFlags |= SEF_PARRIED;
					wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

					if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
					{
						//
					}
					else
					{
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_FIVE);
					}
					if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
						G_ControlledByPlayer(blocker))
					{
						gi.Printf(S_COLOR_RED"Blocker Holding block button only (spamming block) cost 5\n");
					}
				}
				else if (TransitionClashParry || AccurateParry || perfectparry || NPCBlocking) //Other types and npc,s
				{
					if (AccurateParry || perfectparry || NPCBlocking) //Handles NPC blocking
					{
						if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
						{
							if (blocker->client->ps.blockPoints <= BLOCKPOINTS_FOURTY)
							{
								WP_SaberNPCFatiguedParry(blocker, attacker, saberNum, bladeNum);
							}
							else
							{
								WP_SaberNPCParry(blocker, attacker, saberNum, bladeNum);
							}

							if (d_blockinfo->integer && (blocker->NPC && !G_ControlledByPlayer(blocker)))
							{
								gi.Printf(S_COLOR_YELLOW"NPC Fatigued Parry\n");
							}
						}
						else
						{
							WP_SaberParry(blocker, attacker, saberNum, bladeNum);
						}

						G_Sound(blocker, G_SoundIndex(va("sound/weapons/saber/saber_goodparry%d.mp3", Q_irand(1, 3))));

						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_AUTO);
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Other types of block and npc,s\n");
						}
					}
					else if (TransitionClashParry)
					{
						WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);

						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_AUTO);
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Attack Parry heavy slow bounce blocker\n");
						}
					}
					else
					{
						WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_WARNING);
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Just got clipped should drain 7\n");
						}
					}

					blocker->client->ps.saberEventFlags |= SEF_PARRIED;
					wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);
				}
				else
				{
					WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);

					blocker->client->ps.saberEventFlags |= SEF_PARRIED;
					wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

					if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
					{
						//
					}
					else
					{
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
					}
					if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
						G_ControlledByPlayer(blocker))
					{
						gi.Printf(S_COLOR_RED"Blocker Not holding block drain 10\n");
					}
				}
			}
		}
		else
		{
			//just block it //jacesolaris
			if (blocker->client->ps.saberAnimLevel == SS_FAST ||
				blocker->client->ps.saberAnimLevel == SS_MEDIUM ||
				blocker->client->ps.saberAnimLevel == SS_STRONG ||
				blocker->client->ps.saberAnimLevel == SS_DESANN ||
				blocker->client->ps.saberAnimLevel == SS_TAVION ||
				blocker->client->ps.saberAnimLevel == SS_STAFF ||
				blocker->client->ps.saberAnimLevel == SS_DUAL)
			{
				if (ActiveBlocking) //Holding Block Button + attack button
				{
					if (MBlocking) // A perfectly timed block
					{
						WP_SaberMBlock(blocker, attacker, saberNum, bladeNum);

						blocker->client->ps.saberEventFlags |= SEF_PARRIED;
						wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

						wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);

						PM_AddBlockFatigue(&attacker->client->ps, BLOCKPOINTS_TEN);

						blocker->client->ps.saberAttackChainCount = MISHAPLEVEL_NONE;

						if (d_slowmoaction->integer && (blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker)))
						{
							G_StartStasisEffect(blocker, MEF_NO_SPIN, 200, 0.3f, 0);
							CGCam_BlockShakeSP(0.45f, 100);
						}
						G_Sound(blocker,
							G_SoundIndex(va("sound/weapons/saber/saber_perfectblock%d.mp3", Q_irand(1, 3))));
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Perfect block reward 20\n");
						}
						if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
						{
							SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
						}
					}
					else
					{
						//Spamming block + attack buttons
						WP_SaberParry(blocker, attacker, saberNum, bladeNum);

						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_DANGER);

						if (attacker->NPC && !G_ControlledByPlayer(attacker)) //NPC only
						{
							PM_AddBlockFatigue(&attacker->client->ps, BLOCKPOINTS_AUTO);
						}

						if (d_slowmoaction->integer && (blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker)))
						{
							CGCam_BlockShakeSP(0.45f, 100);
						}

						blocker->client->ps.saberEventFlags |= SEF_PARRIED;
						wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Spamming block + attack cost 4\n");
						}
					}
				}
				else if (Blocking && !ActiveBlocking) //Holding block button only (Spamming block)
				{
					WP_SaberBlockedBounceBlock(blocker, attacker, saberNum, bladeNum);

					if (d_slowmoaction->integer && (blocker->s.number < MAX_CLIENTS || G_ControlledByPlayer(blocker)))
					{
						CGCam_BlockShakeSP(0.45f, 100);
					}

					blocker->client->ps.saberEventFlags |= SEF_PARRIED;
					wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

					if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
					{
						//
					}
					else
					{
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_FIVE);
					}
					if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
						G_ControlledByPlayer(blocker))
					{
						gi.Printf(S_COLOR_RED"Blocker Holding block button only (spamming block) cost 5\n");
					}
				}
				else if (TransitionClashParry || AccurateParry || perfectparry || NPCBlocking) //Other types and npc,s
				{
					if (AccurateParry || perfectparry || NPCBlocking) //Handles NPC blocking
					{
						if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
						{
							if (blocker->client->ps.blockPoints <= BLOCKPOINTS_MISSILE)
							{
								WP_SaberNPCParry(blocker, attacker, saberNum, bladeNum);
							}
							else
							{
								WP_SaberNPCMBlock(blocker, attacker, saberNum, bladeNum);
							}

							if (d_blockinfo->integer && (blocker->NPC && !G_ControlledByPlayer(blocker)))
							{
								gi.Printf(S_COLOR_YELLOW"NPC normal Parry\n");
							}
						}
						else
						{
							WP_SaberMBlock(blocker, attacker, saberNum, bladeNum);
						}

						G_Sound(blocker, G_SoundIndex(va("sound/weapons/saber/saber_goodparry%d.mp3", Q_irand(1, 3))));

						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_AUTO);
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Other types of block and npc,s\n");
						}
					}
					else if (TransitionClashParry)
					{
						WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);

						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_AUTO);
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Attack Parry heavy slow bounce blocker\n");
						}
					}
					else
					{
						WP_SaberBlockedBounceBlock(blocker, attacker, saberNum, bladeNum);
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_WARNING);
						if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
							G_ControlledByPlayer(blocker))
						{
							gi.Printf(S_COLOR_RED"Blocker Just got clipped should drain 7\n");
						}
					}

					blocker->client->ps.saberEventFlags |= SEF_PARRIED;
					wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);
				}
				else
				{
					WP_SaberFatiguedParry(blocker, attacker, saberNum, bladeNum);

					blocker->client->ps.saberEventFlags |= SEF_PARRIED;
					wp_saber_clear_damage_for_ent_num(attacker, blocker->s.number, saberNum, bladeNum);

					if (blocker->NPC && !G_ControlledByPlayer(blocker)) //NPC only
					{
						//
					}
					else
					{
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
					}
					if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
						G_ControlledByPlayer(blocker))
					{
						gi.Printf(S_COLOR_RED"Blocker Not holding block drain 10\n");
					}
				}
			}
		}
	}
	else
	{
		//This must be Unblockable
		if (blocker->client->ps.blockPoints < BLOCKPOINTS_TEN)
		{
			//Low points = bad blocks
			SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker, saberNum, bladeNum);
			wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
		}
		else
		{
			//Low points = bad blocks
			G_Stagger(blocker, attacker, qtrue);
			PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
		}
		if ((d_blockinfo->integer || g_DebugSaberCombat->integer) && blocker->s.number < MAX_CLIENTS ||
			G_ControlledByPlayer(blocker))
		{
			gi.Printf(S_COLOR_RED"Attacker must be Unblockable\n");
		}
	}
}

/////////Functions//////////////