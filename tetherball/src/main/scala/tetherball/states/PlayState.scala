package tetherball.states

import skitch.core.{View2D => PlainView2D, Render, SkitchApp, KeyDown}
import skitch.core.managed.{View2D, SkitchState}
import tetherball.Tetherball
import skitch.gfx.{Font, Sprite}
import skitch.{gl, Types, Color}
import tetherball.things._
import skitch.vector.vec
import org.jbox2d.dynamics.World
import org.jbox2d.common.Vec2
import skitch.stage.box2d.{B2DebugView, B2World}
import tetherball.things.FourDirections
import scala.Some
import tetherball.Tetherball.Thing
import org.jbox2d.callbacks.{ContactImpulse, ContactListener}
import org.jbox2d.dynamics.contacts.Contact
import org.jbox2d.collision.Manifold

class PlayState extends SkitchState(Tetherball) with B2World {

	def onEnter = { world.setContactListener(Contacter) }
	def onExit = { world.setContactListener(null) }

	implicit val world:World = new World(new Vec2(0,0))

	val velocityIterations = 40
	val positionIterations = 30

	val backgroundColor = Some(Color(0x222222))

	val arena = new Arena(app.windowRect.scaled(app.projectionScale))

	val guy = new Player(vec(20, 20), FourDirections(KEY_LEFT, KEY_RIGHT, KEY_DOWN, KEY_UP))

	val pole = new Pole

	val ball = new Ball(vec(0, 10))

	val rope = new Rope(30, pole, ball)

	val things = Set(pole, ball, rope, guy)

	val consoleThing = new Thing {

		def render() {
			Color.white.bind()
			gl.scale2(app.projectionScale)
		}

		def update(dt:Float) {}
	}

	val consoleView = View2D( consoleThing )

	val views = Seq(
		View2D(things),
		new B2DebugView(),
		consoleView
	)

	listenTo(guy, ball)

	object Contacter extends ContactListener {

		val thresh = 0.01f

		def polenode(pole:Pole, node:Rope#Node, contact:Contact) {
			node.setTouching(contact.isTouching)
		}

		def beginContact(contact:Contact) {
			val a = contact.getFixtureA.getUserData
			val b = contact.getFixtureB.getUserData

			(a,b) match {
				case (pole:Pole, node:Rope#Node) => polenode(pole, node, contact)
				case (node:Rope#Node, pole:Pole) => polenode(pole, node, contact)
				case _ =>
			}
		}
		def endContact(contact:Contact) {
			val a = contact.getFixtureA.getUserData
			val b = contact.getFixtureB.getUserData
			(a,b) match {
				case (_:Pole, node:Rope#Node) => polenode(pole, node, contact)
				case (node:Rope#Node, _:Pole) => polenode(pole, node, contact)
				case _ =>
			}
		}
		def preSolve(contact:Contact, oldManifold:Manifold) {}
		def postSolve(contact:Contact, impulse:ContactImpulse) {}
	}

}
