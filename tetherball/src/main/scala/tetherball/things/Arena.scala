package tetherball.things

import skitch.{gfx, Color, helpers, Types}
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

class Arena(rect:Rect)(teams:(Team, Team), tether:Tether)(implicit val app:SkitchApp, world:World) extends Thing with ThingManager with EventSink  {

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

	val view:View2D = View2D(camera)(players ++ Seq(pole, ball, tether))

  lazy val camera = new ArenaCamera

	def update(dt:Float) {

	}

	def render() {
		Color(0x000022).bind()
    gfx.fill(false)
		skitch.gfx.rect(zoomBounds(camera.boundsOut))
		skitch.gfx.rect(zoomBounds(camera.boundsIn))
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

  class ArenaCamera extends Camera2D {

    lazy val minDim = math.min(view.windowRect.width, view.windowRect.height)
    lazy val viewTangentDistanceAtUnityZoom = minDim / 2 * app.worldScale

    val zoomInSpeed = 0.0005f
    val zoomOutSpeed = 0.003f
    val zoomAccel = 0.00001f
    var zoomSpeed = 0f

    val boundsIn = 0.70f
    val boundsOut = 0.75f
    def minDistanceForZoom = math.max(tether.idealSlackLength * 1.2f, tether.initialLength * 0.25)
    def maxDistanceForZoom = math.max(tether.idealSlackLength * 2.25f, tether.initialLength * 0.67)

    def minZoom = viewTangentDistanceAtUnityZoom / maxDistanceForZoom
    def maxZoom = viewTangentDistanceAtUnityZoom / minDistanceForZoom

    def zoomIn() {
      if (zoomSpeed < zoomInSpeed) zoomSpeed += zoomAccel
    }

    def zoomOut() {
      if (zoomSpeed > -zoomOutSpeed) zoomSpeed -= zoomAccel
    }

    def zoomStop() {
      zoomSpeed *= 0.99f
    }

    override def update(dt:Float) {
      return
      val moving = (players).toSet
      val (tooFar, local) = moving.partition( t => t.position.length > maxDistanceForZoom )

      if (local.isEmpty) {
        if (zoom > minZoom) zoomOut()
        else zoomStop()
      } else {
        val furthest = local.maxBy(_.position.lengthSquared)
        if (! tooFar.isEmpty && zoom > minZoom) {
          zoomOut()
        } else if (! onScreen(boundsOut, furthest.position) && zoom > minZoom) {
          zoomOut()
        } else if (zoom < maxZoom && tooFar.isEmpty && onScreen(boundsIn, furthest.position)) {
          zoomIn()
        } else {
          zoomStop()
        }
      }

      zoom *= 1 + zoomSpeed


//      zoom = helpers.clamp(zoom)(minZoom, maxZoom)

    }
  }

}
