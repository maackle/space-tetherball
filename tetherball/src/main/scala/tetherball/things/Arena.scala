package tetherball.things

import skitch.{helpers, Types}
import Types._
import org.jbox2d.dynamics.{Filter, BodyType, World}
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.collision.shapes
import skitch.core.Rect

class Arena(width:Real, height:Real)(implicit world:World) {

	def this(rect:Rect)(implicit world:World) = this(rect.width, rect.height)(world)

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

	class Wall(a:vec2, b:vec2) extends EdgeBody(a,b)

	val walls = {

		val x = width / 2
		val y = height / 2
		val corners = Seq(
			vec(-x, -y), vec(x, -y), vec(x, y), vec(-x, y)
		)
		for (seg <- helpers.pairsLoop(corners)) yield {
			val (a,b) = seg
			new Wall(a, b)
		}
	}
}
