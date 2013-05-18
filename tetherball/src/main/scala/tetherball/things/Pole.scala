package tetherball.things

import tetherball.Tetherball.Thing
import skitch.vector.vec2
import skitch.core.components.CircleShape
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.{World, Filter, BodyType}
import tetherball.Tetherball

class Pole(implicit val world:World) extends Thing with ManagedEmbodied with CircleShape {

	val initialPosition = vec2.zero
	val radius = 0.5f

	def render() {

	}

	lazy val body = {
		import Tetherball.Bits._
		val fixture = Embodied.defaults.fixtureDef
		val bodydef = Embodied.defaults.bodyDef

		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius

		bodydef.`type` = BodyType.STATIC
		bodydef.position = initialPosition
		bodydef.linearDamping = 100f
		val body = world.createBody(bodydef)

		val filter = new Filter
		filter.categoryBits = POLE_BIT
		filter.maskBits = 0xffff

		fixture.shape = circle
		fixture.userData = this
		fixture.filter = filter
		fixture.restitution = 0f
		body.createFixture(fixture)
		body
	}

}
