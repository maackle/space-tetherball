package tetherball.things

import skitch.{Color, helpers, Types}
import Types._
import org.jbox2d.dynamics.{Filter, BodyType, World}
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.collision.shapes
import tetherball.Winding
import skitch.core.managed.View2D
import skitch.core._
import tetherball.TetherballGame.Thing
import skitch.core.components.Position2D

class Arena(rect:Rect, camera:Camera2D)(teams:(Team, Team), tether:Tether)(implicit val app:SkitchApp, world:World) extends Thing with EventSink {

	val maxDistanceForZoom = 40f
	private var furthestPoint = vec2.zero

	def pole = tether.pole
	def ball = tether.ball

	def teamCW = teams._1
	def teamCCW = teams._2

	def players = teamCW.players ++ teamCCW.players

	def distance(t:Position2D) = (t.position - pole.position).length
	def zoomBounds(scale:Float) = view.viewportRect.scaled(scale)
	def onScreen(scale:Float, p:vec2) = zoomBounds(scale).hitTest(p)

	val things = players ++ Seq(pole, ball, tether)

	val view = View2D(camera)(things)

	def update(dt:Float) {
		val autozoomSpeed = 0.01f
		things.foreach(_.update(dt))
		val moving = (players :+ ball)
		val (near, far) = moving.partition( t => onScreen(0.75f, t.position) )
		if (far.isEmpty) {
			view.camera.zoom *= 1 + autozoomSpeed
		}
		else {
			val notTooFar = far.filter(distance(_) < maxDistanceForZoom)
			if (! notTooFar.isEmpty) {
				furthestPoint = (notTooFar).maxBy(distance(_)).position
				if(! onScreen(0.85f, furthestPoint)) {
					view.camera.zoom /= 1 + autozoomSpeed
				}
			}

		}

	}

	def render() {
		Color(0x000022).bind()
		skitch.gfx.rect(zoomBounds(0.8f))
	}

	def checkVictory() {
		def isWrappedUp = tether.wrappedRatio > 0.90f

		if (isWrappedUp) {
			declareVictor(tether.winding)
		}
	}

	def declareVictor(winding:Winding.Value) {
		warn("TODO")
	}


	class EdgeBody(a:vec2, b:vec2) extends Embodied {
		val position = (a+b)/2

		def render() {}

		val body = {
			val fixture = Embodied.defaults.fixtureDef
			val bodydef = Embodied.defaults.bodyDef

			val edge = new shapes.EdgeShape()
			edge.set(a,b)

			bodydef.`type` = BodyType.STATIC

			val filter = new Filter
			filter.categoryBits = 0x04
			filter.maskBits = 0xff - 1

			fixture.restitution = 0
			fixture.shape = edge
			fixture.userData = this
			fixture.filter = filter

			val body = world.createBody(bodydef)
			body.createFixture(fixture)
			body
		}
	}

	listenTo(players : _*)

	class Wall(a:vec2, b:vec2) extends EdgeBody(a,b)

	val walls = {

		val x = rect.width / 2
		val y = rect.height / 2
		val corners = Seq(
			vec(-x, -y), vec(x, -y), vec(x, y), vec(-x, y)
		)
		for (seg <- helpers.pairsLoop(corners)) yield {
			val (a,b) = seg
			new Wall(a, b)
		}
	}
}
