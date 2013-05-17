package tetherball.things

import tetherball.Tetherball.Thing
import skitch.vector.vec2
import skitch.core.components.CircleShape
import skitch.stage.box2d.Embodied
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.{World, Filter, BodyType}

class Pole(implicit val world:World) extends Thing with Embodied with CircleShape {

	val position = vec2.zero
	val radius = 0.5f

	def render() {

	}

	lazy val body = {
		val fixture = Embodied.defaults.fixtureDef
		val bodydef = Embodied.defaults.bodyDef

		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		fixture.shape = circle

		bodydef.`type` = BodyType.STATIC
		bodydef.position = position
		val body = world.createBody(bodydef)

		val filter = new Filter
		filter.categoryBits = 0x04
		filter.maskBits = 0xff - 1

		fixture.userData = this
		fixture.filter = filter
		body.createFixture(fixture)
		body
	}

}
