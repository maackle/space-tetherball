package tetherball.things

import tetherball.TetherballGame.Thing
import skitch.core.scalene.core.VBO
import skitch.vector.vec
import skitch.Color
import org.lwjgl.opengl.GL11
import skitch.core.Rect

class Starfield extends Thing {

	val r = 0.01f
	val R = 100f
	val N = 3000
	val starPoints = for {
		i <- (0 to N)
		c = vec.polar.random(R)
	} yield {
			c
	}

	val starQuads = starPoints.flatMap { c =>

		Seq(
			vec(c.x - r, c.y - r),
			vec(c.x + r, c.y - r),
			vec(c.x + r, c.y + r),
			vec(c.x - r, c.y + r)
		)

	}.toArray

	val vbo = VBO.createAndLoad(vertices=starQuads)

	def update(dt:Float) {

	}

	def render() {
		Color.white.bind()
		vbo.draw(GL11.GL_QUADS)
	}
}
