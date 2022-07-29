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
#include "bg_local.h"
#include "w_saber.h"
#include "ai_main.h"

//////////Defines////////////////
extern qboolean BG_SaberInNonIdleDamageMove(const playerState_t* ps, int AnimIndex);
extern qboolean PM_SaberInBounce(int move);
extern qboolean BG_InSlowBounce(const playerState_t* ps);
extern bot_state_t* botstates[MAX_CLIENTS];
extern qboolean PM_SaberInTransitionAny(int move);
extern qboolean PM_SuperBreakWinAnim(int anim);
extern qboolean walk_check(const gentity_t* self);
extern qboolean WP_SabersCheckLock(gentity_t* ent1, gentity_t* ent2);
extern void PM_AddFatigue(playerState_t* ps, int Fatigue);
extern void G_AddVoiceEvent(gentity_t* self, int event, int speakDebounceTime);
extern qboolean npc_is_dark_jedi(const gentity_t* self);
extern saberMoveName_t PM_BrokenParryForParry(int move);
extern saberMoveName_t pm_broken_parry_for_attack(int move);
extern qboolean PM_InGetUp(const playerState_t* ps);
extern qboolean PM_InForceGetUp(const playerState_t* ps);
extern qboolean G_ControlledByPlayer(gentity_t* self);
extern qboolean WP_BrokenBoltBlockKnockBack(gentity_t* victim, gentity_t* attacker, qboolean allowAnyMove);
extern void wp_block_points_regenerate(const gentity_t* self, int override_amt);
extern void PM_AddBlockFatigue(playerState_t* ps, int Fatigue);
extern saberMoveName_t pm_blockthe_attack(int move);
extern int G_BlocktheAttack(int move);
extern saberMoveName_t PM_SaberBounceForAttack(int move);
extern void G_Stagger(gentity_t* hit_ent, gentity_t* atk, qboolean allowAnyMove);
extern qboolean PM_SuperBreakLoseAnim(int anim);
extern qboolean ButterFingers(gentity_t* saberent, gentity_t* saberOwner, const gentity_t* other, const trace_t* tr);
extern qboolean PM_SaberInnonblockableAttack(int anim);
extern qboolean pm_saber_in_special_attack(int anim);
extern qboolean PM_VelocityForBlockedMove(const playerState_t* ps, vec3_t throwDir);
extern qboolean saberKnockOutOfHand(gentity_t* saberent, gentity_t* saberOwner, vec3_t velocity);
extern void PM_VelocityForSaberMove(const playerState_t* ps, vec3_t throw_dir);
extern void G_SaberAttackBounce(gentity_t* self, gentity_t* other, qboolean hitBody);
extern int G_GetParryForBlock(int block);
extern int G_MBlocktheAttack(int move);
extern saberMoveName_t PM_MBlocktheAttack(int move);
extern qboolean WP_SaberMBlock(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberBlockedBounceBlock(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberFatiguedParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberMBlockDirection(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
extern qboolean WP_SaberBlockNonRandom(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
extern qboolean WP_SaberBouncedSaberDirection(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
extern qboolean WP_SaberFatiguedParryDirection(gentity_t* self, vec3_t hitloc, qboolean missileBlock);
extern void wp_block_points_regenerate_over_ride(const gentity_t* self, int override_amt);
extern qboolean WP_SaberNPCParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberNPCMBlock(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
extern qboolean WP_SaberNPCFatiguedParry(gentity_t* blocker, gentity_t* attacker, int saberNum, int bladeNum);
void SabBeh_AnimateHeavySlowBounceAttacker(gentity_t* attacker, gentity_t* Blocker);
extern saberMoveName_t PM_SaberAttackStagger(int move);
extern void G_StaggerAttacker(gentity_t* atk, gentity_t* hitEnt, qboolean allowAnyMove);
extern int wp_saber_must_block(gentity_t* self, const gentity_t* atk, qboolean check_b_box_block, vec3_t point,
	int r_saber_num,
	int r_blade_num);
extern int wp_saber_must_m_block(gentity_t* self, const gentity_t* atk, qboolean check_b_box_block, vec3_t point,
	int r_saber_num,
	int r_blade_num);
//////////Defines////////////////

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

void SabBeh_SaberShouldBeDisarmedAttacker(gentity_t* attacker, const gentity_t* blocker)
{
	static trace_t tr;

	if (!(attacker->client->saber[0].saberFlags & SFL_NOT_DISARMABLE))
	{
		//knocked the saber right out of his hand!
		vec3_t throwDir;
		if (!PM_VelocityForBlockedMove(&blocker->client->ps, throwDir))
		{
			PM_VelocityForSaberMove(&attacker->client->ps, throwDir);
		}
		ButterFingers(&g_entities[attacker->client->ps.saberEntityNum], attacker, blocker, &tr);
	}
}

void SabBeh_SaberShouldBeDisarmedBlocker(gentity_t* blocker, const gentity_t* attacker)
{
	static trace_t tr;

	if (!(blocker->client->saber[0].saberFlags & SFL_NOT_DISARMABLE))
	{
		//knocked the saber right out of his hand!
		vec3_t throwDir;
		if (!PM_VelocityForBlockedMove(&attacker->client->ps, throwDir))
		{
			PM_VelocityForSaberMove(&blocker->client->ps, throwDir);
		}
		ButterFingers(&g_entities[blocker->client->ps.saberEntityNum], blocker, attacker, &tr);
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

	if (!(self->r.svFlags & SVF_BOT))
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

	if (BG_SaberInNonIdleDamageMove(&self->client->ps, self->localAnimIndex)
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
	AngleVectors(p_angles, NULL, p_right, NULL);
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
	if (self->r.svFlags & SVF_BOT)
	{
		//bots just randomly parry to make up for them not intelligently parrying.
		if (BOT_PARRYRATE * botstates[self->s.number]->settings.skill > Q_irand(0, 999))
		{
			return qtrue;
		}
	}

	return qfalse;
}

qboolean g_perfect_blocking(const gentity_t* blocker, const gentity_t* attacker, vec3_t hitLoc)
{
	//determines if self (who is blocking) is actively m blocking (parrying)
	vec3_t p_angles;
	vec3_t p_right;
	vec3_t parrier_move;
	vec3_t hit_pos;
	vec3_t hit_flat; //flatten 2D version of the hitPos.
	const qboolean in_front_of_me = in_front(attacker->client->ps.origin, blocker->client->ps.origin,
		blocker->client->ps.viewangles, 0.0f);

	if (!(blocker->r.svFlags & SVF_BOT))
	{
		if (!(blocker->client->ps.ManualBlockingFlags & 1 << MBF_PROJBLOCKING))
		{
			return qfalse;
		}
	}

	if (!in_front_of_me)
	{
		//can't parry attacks to the rear.
		return qfalse;
	}
	if (PM_SaberInKnockaway(blocker->client->ps.saberMove))
	{
		//already in parry move, continue parrying anything that hits us as long as
		//the attacker is in the same general area that we're facing.
		return qtrue;
	}

	if (blocker->client->ps.ManualblockLastStartTime >= g_SaberPerfectBlockingwaitTimer.integer) //3 sec
	{
		//cant perfect parry if your too slow
		return qfalse;
	}

	if (PM_KickingAnim(blocker->client->ps.legsAnim))
	{
		//can't parry in kick.
		return qfalse;
	}

	if (BG_SaberInNonIdleDamageMove(&blocker->client->ps, blocker->localAnimIndex)
		|| PM_SaberInBounce(blocker->client->ps.saberMove) || BG_InSlowBounce(&blocker->client->ps))
	{
		//can't parry if we're transitioning into a block from an attack state.
		return qfalse;
	}

	if (blocker->client->ps.pm_flags & PMF_DUCKED)
	{
		//can't parry while ducked or running
		return qfalse;
	}

	if (PM_InKnockDown(&blocker->client->ps))
	{
		//can't block while knocked down or getting up from knockdown, or we are staggered.
		return qfalse;
	}

	//set up flatten version of the location of the incoming attack in orientation
	//to the player.
	VectorSubtract(hitLoc, blocker->client->ps.origin, hit_pos);
	VectorSet(p_angles, 0, blocker->client->ps.viewangles[YAW], 0);
	AngleVectors(p_angles, NULL, p_right, NULL);
	hit_flat[0] = 0;
	hit_flat[1] = DotProduct(p_right, hit_pos);

	//just bump the hit pos down for the blocking since the average left/right slice happens at about origin +10
	hit_flat[2] = hit_pos[2] - 10;
	VectorNormalize(hit_flat);

	//set up the vector for the direction the player is trying to parry in.
	parrier_move[0] = 0;
	parrier_move[1] = blocker->client->pers.cmd.rightmove;
	parrier_move[2] = -blocker->client->pers.cmd.forwardmove;
	VectorNormalize(parrier_move);

	const float block_dot = DotProduct(hit_flat, parrier_move);

	if (block_dot >= .4)
	{
		//player successfully blocked in the right direction
		return qtrue;
	}
	//player didn't parry in the correct direction, do blockPoints punishment
	blocker->client->ps.fd.blockPoints -= BLOCKPOINTS_FAIL;

	if (blocker->r.svFlags & SVF_BOT)
	{
		//bots just randomly parry to make up for them not intelligently parrying.
		if (BOT_PARRYRATE * botstates[blocker->s.number]->settings.skill > Q_irand(0, 999))
		{
			return qtrue;
		}
	}
	else if (blocker->s.eType == ET_NPC)
	{
		if (BOT_PARRYRATE * g_npcspskill.integer > Q_irand(0, 999))
		{
			return qtrue;
		}
	}

	return qfalse;
}

void SabBeh_AddMishap_Attacker(gentity_t* attacker, gentity_t* blocker)
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
			if (d_attackinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_YELLOW"Attacker staggering\n");
			}
			break;
		case 1:
			SabBeh_SaberShouldBeDisarmedAttacker(attacker, blocker);
			if (d_attackinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_RED"Attacker lost his saber\n");
			}
			break;
		default:;
		}
	}
}

void SabBeh_AddMishap_Blocker(gentity_t* blocker, gentity_t* attacker)
{
	if (blocker->client->ps.saberAttackChainCount <= MISHAPLEVEL_NONE)
	{
		blocker->client->ps.saberAttackChainCount = MISHAPLEVEL_NONE;
	}
	else
	{
		//overflowing causes a full mishap.
		const int randNum = Q_irand(0, 2);

		switch (randNum)
		{
		case 0:
			G_Stagger(blocker, attacker, qtrue);
			if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(blocker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_YELLOW"blocker staggering\n");
			}
			break;
		case 1:
			SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker);
			if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(blocker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_RED"blocker lost his saber\n");
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

qboolean SabBeh_RollBalance_Advanced(gentity_t* attacker, gentity_t* blocker, qboolean forceMishap)
{
	//JaceSolaris making it feel like EoC MP/OJP
	//if the attack is blocked -(Im the attacker)

	if (attacker->client->ps.saberAttackChainCount >= MISHAPLEVEL_FULL)
	{
		//hard mishap.
		if (attacker->r.svFlags & SVF_BOT) //NPC only
		{
			SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
			if ((d_attackinfo.integer || g_DebugSaberCombat.integer) && !(attacker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_GREEN"npc attacker is fatigued\n");
			}
		}
		else
		{
			SabBeh_AddMishap_Attacker(attacker, blocker);
			if ((d_attackinfo.integer || g_DebugSaberCombat.integer) && !(attacker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_GREEN"Player hit full mishap\n");
			}
		}
		attacker->client->ps.saberAttackChainCount = MISHAPLEVEL_LIGHT;
		return qtrue;
	}
	if (attacker->client->ps.saberAttackChainCount >= MISHAPLEVEL_HUDFLASH)
	{
		//slow bounce
		SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
		attacker->client->ps.saberAttackChainCount = MISHAPLEVEL_LIGHT;
		if ((d_attackinfo.integer || g_DebugSaberCombat.integer) && !(attacker->r.svFlags & SVF_BOT))
		{
			Com_Printf(S_COLOR_GREEN"Attacker stagger\n");
		}
		return qtrue;
	}
	if (attacker->client->ps.saberAttackChainCount >= MISHAPLEVEL_LIGHT)
	{
		//slow bounce
		SabBeh_AnimateSmallBounce(attacker, blocker);
		if ((d_attackinfo.integer || g_DebugSaberCombat.integer) && !(attacker->r.svFlags & SVF_BOT))
		{
			Com_Printf(S_COLOR_GREEN"Attacker small bounce\n");
		}
		return qtrue;
	}
	if (forceMishap)
	{
		//perform a slow bounce even if we don't have enough mishap for it.
		SabBeh_AnimateSmallBounce(attacker, blocker);
		if ((d_attackinfo.integer || g_DebugSaberCombat.integer) && !(attacker->r.svFlags & SVF_BOT))
		{
			Com_Printf(S_COLOR_GREEN"Attacker blocked bounce\n");
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

void SabBeh_AttackVsAttack(gentity_t* attacker, gentity_t* blocker)
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
	else if (PM_SaberInKata(attacker->client->ps.saberMove))
	{
		SabBeh_AddBalance(attacker, 1);
		//set otherOwner
		SabBeh_AddBalance(blocker, -1);

		if (blocker->client->ps.fd.blockPoints < BLOCKPOINTS_TEN)
		{
			//Low points = bad blocks
			SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker);
			wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
		}
		else
		{
			//Low points = bad blocks
			G_Stagger(blocker, attacker, qtrue);
			PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
		}
	}
	else if (PM_SaberInKata(blocker->client->ps.saberMove))
	{
		SabBeh_AddBalance(attacker, -1);
		//set otherOwner
		SabBeh_AddBalance(blocker, 1);

		if (attacker->client->ps.fd.blockPoints < BLOCKPOINTS_TEN)
		{
			//Low points = bad blocks
			SabBeh_SaberShouldBeDisarmedAttacker(attacker, blocker);
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
		//either both are faking or neither is faking.  Either way, it's cancelled out
		//set self
		SabBeh_AddBalance(attacker, 1);
		//set otherOwner
		SabBeh_AddBalance(blocker, 1);

		SabBeh_RollBalance_Advanced(attacker, blocker, qtrue);

		SabBeh_RollBalance_Advanced(blocker, attacker, qtrue);
	}
}

void SabBeh_AttackvBlock(gentity_t* attacker, gentity_t* blocker, int saberNum, int bladeNum, vec3_t hitLoc,
	qboolean hitSaberBlade)
{
	//if the attack is blocked -(Im the attacker)
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
	qboolean startSaberLock = qfalse;

	if ((AccurateParry || perfectparry) && blocker->r.svFlags & SVF_BOT
		&& BOT_ATTACKPARRYRATE * botstates[blocker->s.number]->settings.skill > Q_irand(0, 999))
	{
		//npc performed an attack parry (by cheating a bit)
		TransitionClashParry = qtrue;
	}

	if (!PM_SaberInnonblockableAttack(attacker->client->ps.torsoAnim))
	{
		if (BG_SaberInNonIdleDamageMove(&blocker->client->ps, blocker->localAnimIndex) || TransitionClashParry)
			//Blocker is not blocking, he is attacking also
		{
			SabBeh_AttackVsAttack(attacker, blocker);
			if (d_combatinfo.integer)
			{
				Com_Printf(S_COLOR_YELLOW"Attacker SabBeh_AttackVsAttack\n");
			}
		}
		else if (PM_SuperBreakWinAnim(attacker->client->ps.torsoAnim))
		{
			//attacker was attempting a superbreak and he hit someone who could block the move, rail him for screwing up.
			SabBeh_RollBalance_Advanced(attacker, blocker, qtrue);
			SabBeh_AddBalance(attacker, 1);

			SabBeh_AddBalance(blocker, -1);
			if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(blocker->r.svFlags & SVF_BOT))
			{
				Com_Printf(S_COLOR_YELLOW"Attacker Super break win / fail\n");
			}
		}
		else if (atkfake)
		{
			//attacker faked before making this attack, treat like standard attack/attack
			if (AccurateParry || perfectparry || Blocking || MBlocking || ActiveBlocking || NPCBlocking)
			{
				//defender parried the attack fake.
				SabBeh_RollBalance_Advanced(attacker, blocker, qtrue);
				SabBeh_AddBalance(attacker, MPCOST_PARRIED_ATTACKFAKE);

				if (Blocking || MBlocking || ActiveBlocking || NPCBlocking)
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_QUICKPARRY;
				}
				else
				{
					attacker->client->ps.userInt3 |= 1 << FLAG_PARRIED;
				}

				SabBeh_AddBalance(blocker, MPCOST_PARRYING_ATTACKFAKE);
				if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(blocker->r.svFlags & SVF_BOT))
				{
					Com_Printf(S_COLOR_YELLOW"Attacker Attack Fake Blocked\n");
				}
			}
			else
			{
				//otherwise, the defender stands a good chance of having his defenses broken.
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
					startSaberLock = qtrue;
				}

				G_SaberAttackBounce(attacker, blocker, qfalse);
				if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
				{
					Com_Printf(S_COLOR_YELLOW"Attacker Attack Fake bounced\n");
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

				if ((Blocking || MBlocking || NPCBlocking) && attacker->client->ps.fd.blockPoints <= BLOCKPOINTS_TEN)
				{
					SabBeh_AddMishap_Attacker(attacker, blocker);
					if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
					{
						Com_Printf(S_COLOR_YELLOW"Blocker disarmed attacker\n");
					}
					wp_block_points_regenerate(attacker, BLOCKPOINTS_FATIGUE);
				}
				else if ((Blocking || NPCBlocking) && !MBlocking && !ActiveBlocking)
				{
					SabBeh_RollBalance_Advanced(attacker, blocker, qtrue);

					if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
					{
						if (NPCBlocking)
						{
							Com_Printf(S_COLOR_YELLOW"Attackers Blocker npc is using Block button spam\n");
						}
						else
						{
							Com_Printf(S_COLOR_YELLOW"Attackers Blocker is using Block button spam\n");
						}
					}
				}
				else
				{
					SabBeh_RollBalance_Advanced(attacker, blocker, qtrue);

					if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
					{
						Com_Printf(S_COLOR_YELLOW"Attacker Most attacks handled here\n");
					}
				}
			}
			else
			{
				//Backup in case i missed some
				SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);

				SabBeh_AnimateHeavySlowBounceBlocker(blocker, attacker);

				PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);

				if (d_combatinfo.integer || g_DebugSaberCombat.integer && !(attacker->r.svFlags & SVF_BOT))
				{
					Com_Printf(S_COLOR_MAGENTA"Attacker All the rest of the types of contact\n");
				}
			}
		}

		if ((!OnSameTeam(attacker, blocker) || g_friendlySaber.integer) && !startSaberLock)
		{
			if (wp_saber_must_block(blocker, attacker, qfalse, hitLoc, -1, -1)) //Holding Block Button only
			{
				if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags & SVF_BOT))
				{
					Com_Printf(S_COLOR_CYAN"Players Blocking starts here 1\n");
				}

				if (!PM_SaberInnonblockableAttack(attacker->client->ps.torsoAnim))
				{
					if (wp_saber_must_m_block(blocker, attacker, qfalse, hitLoc, -1, -1))
						//Holding Block Button + attack button
					{
						if (blocker->client->ps.ManualBlockingFlags & 1 << MBF_MBLOCKING) //Perfect blocking
						{
							WP_SaberMBlockDirection(blocker, hitLoc, qfalse);

							wp_block_points_regenerate(blocker, BLOCKPOINTS_FATIGUE);
							blocker->client->ps.saberAttackChainCount = MISHAPLEVEL_NONE;
							G_Sound(blocker, CHAN_AUTO,
								G_SoundIndex(va("sound/weapons/saber/saber_perfectblock%d.mp3", Q_irand(1, 3))));

							if (!(blocker->r.svFlags & SVF_BOT))
							{
								CGCam_BlockShakeMP(blocker->s.origin, NULL, 0.45f, 100, qfalse);
							}

							if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags & SVF_BOT))
							{
								Com_Printf(S_COLOR_RED"Perfect Block regen 20bp\n");
							}
							if (attacker->r.svFlags & SVF_BOT)
							{
								SabBeh_AddMishap_Attacker(attacker, blocker);
							}
							else
							{
								SabBeh_AnimateHeavySlowBounceAttacker(attacker, blocker);
							}
						}
						else if (blocker->client->ps.ManualBlockingFlags & 1 << MBF_NPCBLOCKING)
							//Handles NPC blocking
						{
							if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_HALF)
							{
								if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_FOURTY)
								{
									WP_SaberFatiguedParryDirection(blocker, hitLoc, qfalse);
								}
								else
								{
									WP_SaberBlockNonRandom(blocker, hitLoc, qfalse);
								}

								if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && blocker->r.svFlags &
									SVF_BOT)
								{
									Com_Printf(S_COLOR_YELLOW"NPC Fatigued Parry\n");
								}
							}
							else
							{
								WP_SaberMBlockDirection(blocker, hitLoc, qfalse);

								if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && blocker->r.svFlags &
									SVF_BOT)
								{
									Com_Printf(S_COLOR_GREY"NPC normal Parry\n");
								}
							}

							PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_DANGER);

							if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags & SVF_BOT))
							{
								Com_Printf(S_COLOR_CYAN"npc blocking\n");
							}
						}
						else
						{
							//Spamming block + attack buttons
							if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_FATIGUE)
							{
								if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_TEN)
								{
									SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker);

									if (blocker->client->ps.fd.blockPoints <= MISHAPLEVEL_NONE)
									{
										blocker->client->ps.fd.blockPoints = MISHAPLEVEL_NONE;
									}
									else
									{
										wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
									}
									if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags &
										SVF_BOT))
									{
										Com_Printf(S_COLOR_CYAN"Blocker Drop saber recharge to 20bp\n");
									}
								}
								else
								{
									//Low points = bad blocks
									G_Stagger(blocker, attacker, qtrue);

									PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_DANGER);

									if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags &
										SVF_BOT))
									{
										Com_Printf(S_COLOR_CYAN"Blocker stagger drain 4 bp\n");
									}
								}
							}
							else if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_HALF)
							{
								WP_SaberFatiguedParryDirection(blocker, hitLoc, qfalse);

								PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_AUTO);

								if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags &
									SVF_BOT))
								{
									Com_Printf(S_COLOR_CYAN"Holding Block Button + attack button drain 3bp\n");
								}
							}
							else
							{
								WP_SaberBlockNonRandom(blocker, hitLoc, qfalse);

								if (!(blocker->r.svFlags & SVF_BOT))
								{
									CGCam_BlockShakeMP(blocker->s.origin, NULL, 0.45f, 100, qfalse);
								}

								PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_AUTO);

								if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags &
									SVF_BOT))
								{
									Com_Printf(S_COLOR_CYAN"Holding Block Button + attack button drain 3bp\n");
								}
							}
						}
					}
					else if (blocker->client->ps.ManualBlockingFlags & 1 << MBF_BLOCKING && !(blocker->client->ps.
						ManualBlockingFlags & 1 << MBF_PROJBLOCKING)) //Holding block button only (spamming block)
					{
						if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_FATIGUE)
						{
							if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_TEN)
							{
								SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker);

								if (blocker->client->ps.fd.blockPoints <= MISHAPLEVEL_NONE)
								{
									blocker->client->ps.fd.blockPoints = MISHAPLEVEL_NONE;
								}
								else
								{
									wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
								}
								if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags &
									SVF_BOT))
								{
									Com_Printf(S_COLOR_CYAN"Blocker Drop saber recharge to 20bp\n");
								}
							}
							else
							{
								//Low points = bad blocks
								G_Stagger(blocker, attacker, qtrue);

								PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_DANGER);

								if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags &
									SVF_BOT))
								{
									Com_Printf(S_COLOR_CYAN"Blocker stagger drain 4 bp\n");
								}
							}
						}
						else if (blocker->client->ps.fd.blockPoints <= BLOCKPOINTS_HALF)
						{
							WP_SaberFatiguedParryDirection(blocker, hitLoc, qfalse);

							PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_FIVE);

							if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags & SVF_BOT))
							{
								Com_Printf(S_COLOR_CYAN"Holding Block Button button drain 5bp\n");
							}
						}
						else
						{
							WP_SaberBouncedSaberDirection(blocker, hitLoc, qfalse);

							PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_FIVE);

							if (d_blockinfo.integer && !(blocker->r.svFlags & SVF_BOT))
							{
								Com_Printf(S_COLOR_CYAN"Holding Block Button button drain 5bp\n");
							}
						}
					}
					else
					{
						WP_SaberBouncedSaberDirection(blocker, hitLoc, qfalse);
						if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags & SVF_BOT))
						{
							Com_Printf(S_COLOR_CYAN"Not Blocking but saber hit do mishap\n");
						}
					}
				}
				else
				{
					//This must be Unblockable
					if (blocker->client->ps.fd.blockPoints < BLOCKPOINTS_TEN)
					{
						//Low points = bad blocks
						SabBeh_SaberShouldBeDisarmedBlocker(blocker, attacker);
						wp_block_points_regenerate_over_ride(blocker, BLOCKPOINTS_FATIGUE);
					}
					else
					{
						//Low points = bad blocks
						G_Stagger(blocker, attacker, qtrue);
						PM_AddBlockFatigue(&blocker->client->ps, BLOCKPOINTS_TEN);
					}
					if ((d_blockinfo.integer || g_DebugSaberCombat.integer) && !(blocker->r.svFlags & SVF_BOT))
					{
						Com_Printf(S_COLOR_MAGENTA"Attacker must be Unblockable\n");
					}
				}
			}
		}
	}
}

/////////Functions//////////////