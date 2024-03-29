package adventofcode.y2015

import scala.annotation.tailrec
import scala.io.BufferedSource

object Day22 extends Year2015 {
  override val day: Int = 22

  private val spells = Set(MagicMissile, Drain, Shield, Poison, Recharge)

  override def runDay(input: BufferedSource): Unit = {
    val bossStats = input.getLines().map(_.split(':')(1).trim.toInt)
    val boss = Boss(bossStats.next(), bossStats.next())

    printDayPart(1, simulate(Seq(GameState(Wizard(50, 0, 500), boss, 0, Seq())), Int.MaxValue, hard = false),
      "least amount of mana: %s")
    printDayPart(2, simulate(Seq(GameState(Wizard(50, 0, 500), boss, 0, Seq())), Int.MaxValue, hard = true),
      "least amount of mana in hard mode: %s")
  }

  @tailrec
  private def simulate(games: Seq[GameState], bestGameMana: Int, hard: Boolean): Int = games match {
    case Seq() => bestGameMana
    case _ =>
      val playerTurns = games.flatMap(game => spells.filter(_.possible(game)).map(game.playerTurn(_, hard)))
      val score = findBestGameMana(bestGameMana, playerTurns)
      val bossTurns = filterGamesToCheck(score, playerTurns).map(_.bossTurn)
      simulate(filterGamesToCheck(score, bossTurns), findBestGameMana(score, bossTurns), hard)
  }

  private def findBestGameMana(bestGame: Int, games: Seq[GameState]) =
    games.filter(_.won).sortBy(_.usedMana).headOption.map(_.usedMana).getOrElse(bestGame).min(bestGame)

  private def filterGamesToCheck(bestGame: Int, games: Seq[GameState]) =
    games.filter(game => game.running && game.usedMana < bestGame)

  private case class Wizard(hitPoints: Int, armor: Int, mana: Int) {
    def takeDamage(damage: Int): Wizard = copy(hitPoints = hitPoints - (damage - armor).max(1))
    def heal(heal: Int): Wizard = copy(hitPoints = hitPoints + heal)
    def setArmor(newArmor: Int): Wizard = copy(armor = newArmor)
    def changeMana(manaChange: Int): Wizard = copy(mana = mana + manaChange)
  }
  private case class Boss(hitPoints: Int, damage: Int) {
    def takeDamage(damage: Int): Boss = copy(hitPoints = hitPoints - damage)
  }
  private case class GameState(wizard: Wizard, boss: Boss, usedMana: Int, activeEffects: Seq[Effect]) {
    val activeEffectsForTurn: Seq[Effect] = activeEffects.map(_.timerCountDown)

    def won: Boolean = boss.hitPoints <= 0
    def running: Boolean = !won && wizard.hitPoints > 0

    def playerTurn(spell: Spell, hard: Boolean): GameState = {
      val hardModeWizard = if (hard) wizard.takeDamage(1) else wizard
      if (hard && hardModeWizard.hitPoints <= 0)
        return copy(wizard = hardModeWizard)

      val (wizardAfterEffects, bossAfterEffects) = applyEffect(hardModeWizard, boss)
      val (wizardAfterCast, bossAfterCast) = spell.cast(wizardAfterEffects, bossAfterEffects)

      GameState(wizardAfterCast.changeMana(-spell.cost), bossAfterCast, usedMana + spell.cost, spell.effect.foldLeft(filterEffects)(_ :+ _))
    }

    def bossTurn: GameState = {
      val (wizardAfterEffects, bossAfterEffects) = applyEffect(wizard, boss)

      copy(wizard = wizardAfterEffects.takeDamage(boss.damage),
        boss = bossAfterEffects, activeEffects = filterEffects)
    }

    private def applyEffect(wizard: Wizard, boss: Boss) =
      activeEffectsForTurn.foldLeft(wizard, boss)((params, effect) => effect.applyEffect(params._1, params._2))

    private def filterEffects = activeEffectsForTurn.filter(_.timer > 0)
  }

  // spells
  private case class Spell(cost: Int, effect: Option[Effect]) {
    def cast(wizard: Wizard, boss: Boss): (Wizard, Boss) = (wizard, boss)
    def possible(game: GameState): Boolean = cost <= game.wizard.mana &&
      effect.forall(effect =>
        !game.activeEffectsForTurn.exists(gameEffect =>
          gameEffect.timer > 0 && gameEffect.effect == effect.effect)
      )
  }
  private object MagicMissile extends Spell(53, None) {
    override def cast(wizard: Wizard, boss: Boss): (Wizard, Boss) = (wizard, boss.takeDamage(4))
  }
  private object Drain extends Spell(73, None) {
    override def cast(wizard: Wizard, boss: Boss): (Wizard, Boss) = (wizard.heal(2), boss.takeDamage(2))
  }
  private object Shield extends Spell(113, Some(Effect(6, ShieldEffect))) {
    override def cast(wizard: Wizard, boss: Boss): (Wizard, Boss) = (wizard.setArmor(7), boss)
  }
  private object Poison extends Spell(173, Some(Effect(6, PoisonEffect)))
  private object Recharge extends Spell(229, Some(Effect(5, RechargeEffect)))

  // effects
  private case class Effect(timer: Int, effect: EffectLogic) {
    def timerCountDown: Effect = copy(timer = timer - 1)
    def applyEffect(wizard: Wizard, boss: Boss): (Wizard, Boss) = effect.applyEffect(wizard, boss, timer)
  }
  private abstract class EffectLogic {
    def applyEffect(wizard: Wizard, boss: Boss, timer: Int): (Wizard, Boss)
  }
  private object ShieldEffect extends EffectLogic {
    override def applyEffect(wizard: Wizard, boss: Boss, timer: Int): (Wizard, Boss) = (if (timer == 0) wizard.setArmor(0) else wizard, boss)
  }
  private object PoisonEffect extends EffectLogic {
    override def applyEffect(wizard: Wizard, boss: Boss, timer: Int): (Wizard, Boss) = (wizard, boss.takeDamage(3))
  }
  private object RechargeEffect extends EffectLogic {
    override def applyEffect(wizard: Wizard, boss: Boss, timer: Int): (Wizard, Boss) = (wizard.changeMana(101), boss)
  }
}
