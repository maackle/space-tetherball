package tetherball.things

import tetherball.Tetherball.Thing
import skitch.vector.vec2
import skitch.core.managed.components.Position2D
import org.jbox2d.dynamics._
import tetherball.{Tetherball}
import org.jbox2d.collision.shapes
import skitch.core.components.CircleShape
import skitch.gfx.{SpriteLike, Sprite}
import skitch.core.{SkitchApp, ResourceLike}
import skitch.stage.box2d.{B2World, B2Body}

object Ball {

}

class Ball(val position:vec2)(implicit val world:World, val app:SkitchApp) extends Thing with Sprite with B2Body with CircleShape {

	val image = Tetherball.loader.image("img/rgbtest.png")

	def radius = dimensions.x / 2

	lazy val body = {
		import B2Body.defaults._
		val fixture = B2Body.defaults.fixtureDef
		val bodydef = B2Body.defaults.bodyDef

		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		fixture.shape = circle

		bodydef.`type` = BodyType.DYNAMIC
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
