package tetherball.things

import skitch.{Color, helpers, Types}
import Types._
import org.jbox2d.dynamics.{Filter, BodyType, World}
import skitch.stage.box2d.{EdgeBody, Embodied, ManagedEmbodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.collision.shapes
import tetherball.Winding
import skitch.core.managed.{ThingManager, View2D}
import skitch.core._
import tetherball.TetherballGame.Thing
import skitch.core.components.Position2D

class Arena(rect:Rect, camera:Camera2D)(teams:(Team, Team), tether:Tether)(implicit val app:SkitchApp, world:World) extends Thing with ThingManager with EventSink  {

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

	val things = players ++ players.map(_.OffScreenMarker) ++ Seq(pole, ball, tether)

	val view = View2D(camera)(things)

	def autozoom() = {
		val autozoomSpeed = 0.01f
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

	def update(dt:Float) {
		autozoom()
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
