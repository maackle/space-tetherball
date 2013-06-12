package tetherball.states

import skitch.core.{View2D => PlainView2D, _}
import skitch.core.managed.{View2D, SkitchState}
import tetherball.{Winding, TetherballGame}
import skitch.gfx.{Font, Sprite}
import skitch.{gl, Types, Color}
import tetherball.things._
import skitch.vector.vec
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

class PlayState extends SkitchState(TetherballGame) with B2World {

	def onEnter = { world.setContactListener(Contacter) }
	def onExit = { world.setContactListener(null) }

	implicit val world:World = new World(new Vec2(0,0))

	val baseVelocityIterations = 10
	val basePositionIterations = 3
	val iterationIncrement = 100
	val maxIterations = 500
	var velocityIterations = baseVelocityIterations
	var positionIterations = basePositionIterations

	val clearColor = Some(Color(0x222222))

	val player1 = new Player(vec(-20, 20), Player.Controls(KEY_LEFT, KEY_RIGHT, KEY_DOWN, KEY_UP, KEY_SPACE))
	val player2 = new Player(vec(20, 20), Player.Controls(KEY_NUMPAD4, KEY_NUMPAD6, KEY_NUMPAD5, KEY_NUMPAD8, KEY_NUMPADENTER))

	val pole = new Pole(0.6f)

	val ball = new Ball(vec(0, 20))

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

	val camera = new Camera2D

	val teams = (
		Team(Winding.CW, Seq(player1)),
		Team(Winding.CCW, Seq(player2))
	)

	val arena = new Arena(app.windowRect.scaled(app.projectionScale * 2), camera)(teams, tether)

	def things = Seq(arena) ++ arena.things

	val view = arena.view

	val consoleView = View2D(new Camera2D)( Seq(consoleThing) )

	val views = Seq(
		view,
		new B2DebugView(camera)
		//		consoleView
	)

	private var paused = false
	private var advance = false

	val panning = 0.25f

	listen {
		case KeyHold(KEY_EQUALS) => camera.zoom *= 1.05f
		case KeyHold(KEY_MINUS) => camera.zoom /= 1.05f

		case KeyHold(KEY_J) => camera.position.x -= panning
		case KeyHold(KEY_L) => camera.position.x += panning
		case KeyHold(KEY_K) => camera.position.y -= panning
		case KeyHold(KEY_I) => camera.position.y += panning

		case KeyDown(KEY_NUMPAD0) =>
//			app.pushState(new PauseState(this))
			paused = ! paused
		case KeyDown(KEY_PERIOD) =>
			if(paused) advance = true
	}

	listenTo(arena, ball)

	override def update(dt:Float) {
		handlePause(dt)
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
