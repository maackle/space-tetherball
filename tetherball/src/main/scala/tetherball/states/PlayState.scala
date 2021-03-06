package tetherball.states

import skitch.core.{View2D => PlainView2D, _}
import skitch.core.managed.{View2D, SkitchState}
import tetherball.{Winding, TetherballGame}
import skitch.gfx.{Font, Sprite}
import skitch.{gl, Types, Color}
import tetherball.things._
import skitch.vector.{vec2, vec}
import org.jbox2d.dynamics.{BodyType, World}
import org.jbox2d.common.Vec2
import skitch.stage.box2d.{Embodied, B2DebugView, B2World}
import scala.Some
import tetherball.TetherballGame.Thing
import org.jbox2d.callbacks.{ContactImpulse, ContactListener}
import org.jbox2d.dynamics.contacts.Contact
import org.jbox2d.collision.Manifold
import skitch.core.KeyDown
import scala.Some
import skitch.core.KeyHold
import skitch.input.{X360, ControllerSystem}

class PlayState extends SkitchState(TetherballGame) with B2World {

	def onEnter = {
    world.setContactListener(Contacter)
    TetherballGame.loader.loadAll()
  }
	def onExit = { world.setContactListener(null) }

	implicit val world:World = new World(new Vec2(0,0))
	implicit val state = this

	val baseVelocityIterations = 30
	val basePositionIterations = 8
	val iterationIncrement = 100
	val maxIterations = 500
	var velocityIterations = baseVelocityIterations
	var positionIterations = basePositionIterations

  val numPlayers = 2

	val clearColor = Some(Color(0x222222))

  val defaultKeyboardControls = IndexedSeq(
    Player.KeyboardControls(KEY_A, KEY_D, KEY_S, KEY_W, KEY_SPACE),
    Player.KeyboardControls(KEY_LEFT, KEY_RIGHT, KEY_DOWN, KEY_UP, KEY_RETURN)
  )

  val playerStartPostions = IndexedSeq(
    vec(-20, 0),
    vec(20, 0)
  )

  val players = for (i <- Seq.range(0, numPlayers)) yield {
    val ctls = ControllerSystem.X360Controllers
    val playerControls = {
      if (ctls.length > i) {
        Player.XBoxControls(ctls(i))
      }
      else defaultKeyboardControls(i)
    }
    new Player(playerStartPostions(i), playerControls)
  }

	val pole = new Pole(0.6f)

	val ball = new Ball(vec(0, 20))

	val starfield = new Starfield(500, 50f)

	val tether = new Tether(40, pole, ball)

	val consoleThing = new Thing {

		val font = new Font("font/UbuntuMono-R.ttf", 24)

		def render() {
			Color.white.bind()
			font.drawString((velocityIterations, positionIterations).toString, vec(-2, -1))
			font.drawString("%2.5f".format(tether.error), vec(-3, 5))
		}

		def update(dt:Float) {}
	}

//  override val eventSources = super.eventSources ++: ControllerSystem.X360Controllers

	val teams = (
		Team(Winding.CW, Color.magenta),
		Team(Winding.CCW, Color.blue)
	)

  val (teamCW, teamCCW) = teams

  for ((p, i) <- players.zipWithIndex) {
    if ((i % 2) == 0) p.setTeam(teamCW)
    else p.setTeam(teamCCW)
  }

	val arena = new Arena(Rect(vec2.zero, app.windowRect.width * 4 * app.worldScale, app.windowRect.height * 4 * app.worldScale))(players, teams, tether)

  val camera = arena.camera

	def things = Seq(arena)// ++ arena.things

	val view:View2D = arena.view

	val consoleView = View2D(new Camera2D)( Seq(consoleThing) )

  val HUDView = View2D(new Camera2D)(players.map(_.OffScreenMarker) )

	val views = Seq(
		view,
		View2D(camera)(Seq(arena)),
		View2D(camera)(Seq(starfield)),
		new B2DebugView(camera),
		HUDView
	)

	private var paused = false
	private var slowdown = false
	private var advance = false

	val panning = 0.25f

	listen {
		case KeyHold(KEY_EQUALS) => camera.zoom *= 1.05f
		case KeyHold(KEY_MINUS) => camera.zoom /= 1.05f

		case KeyHold(KEY_J) => camera.position.x -= panning
		case KeyHold(KEY_L) => camera.position.x += panning
		case KeyHold(KEY_K) => camera.position.y -= panning
		case KeyHold(KEY_I) => camera.position.y += panning

		case KeyHold(KEY_COMMA) => slowdown = true

		case KeyDown(KEY_NUMPAD0) =>
//			app.pushState(new PauseState(this))
			paused = ! paused
		case KeyDown(KEY_PERIOD) =>
			if(paused) advance = true
	}

	listenTo(arena, ball)

//  for (p <- players) listenTo(p)

	override def update(dt:Float) {
    if (slowdown) {
      super.update(dt * 0.10f)
    } else {
      super.update(dt)
    }
//		handlePause(dt)
    slowdown = false
	}

	object Contacter extends ContactListener {

		val thresh = 0.01f

		def pole_node(pole:Pole, node:Tether#Node, contact:Contact) {
			node.setTouching(contact.isTouching)
		}

		def beginContact(contact:Contact) {

			def player_other(player:Player, other:Physical) {
				if (other.body.getType == BodyType.DYNAMIC && player.Puff.isActive)
					other.takePuff(player)
				player.SFX.hit.play(true)
			}

			def pole_ball(pole:Pole, ball:Ball) {
				arena.checkVictory()
			}

			val a = contact.getFixtureA.getUserData
			val b = contact.getFixtureB.getUserData

			(a,b) match {
				case (pole:Pole, node:Tether#Node) => pole_node(pole, node, contact)
				case (node:Tether#Node, pole:Pole) => pole_node(pole, node, contact)
				case _ =>
			}
			if(contact.isTouching) {
				(a,b) match {
					case (pole:Pole, ball:Ball) => pole_ball(pole, ball)
					case (ball:Ball, pole:Pole) => pole_ball(pole, ball)
					case (p:Player, q:Player) =>
						player_other(p, q)
						player_other(q, p)
					case (player:Player, other:Physical) => player_other(player, other)
					case (other:Physical, player:Player) => player_other(player, other)
					case _ =>
				}
			}
		}
		def endContact(contact:Contact) {
			val a = contact.getFixtureA.getUserData
			val b = contact.getFixtureB.getUserData
			(a,b) match {
				case (_:Pole, node:Tether#Node) => pole_node(pole, node, contact)
				case (node:Tether#Node, _:Pole) => pole_node(pole, node, contact)
				case _ =>
			}
		}
		def preSolve(contact:Contact, oldManifold:Manifold) {}
		def postSolve(contact:Contact, impulse:ContactImpulse) {}
	}

	def handlePause(dt:Float) {
		if(!paused || advance) {
			if (tether.error > 0.025f) {
				velocityIterations += iterationIncrement
				//				positionIterations += iterationIncrement
			}
			else {
				velocityIterations = baseVelocityIterations
				//				positionIterations = basePositionIterations
			}
			velocityIterations = math.min(maxIterations, velocityIterations)
			//			positionIterations = math.min(maxIterations, positionIterations)
			super.update(dt)
		}
		advance = false
	}

}
