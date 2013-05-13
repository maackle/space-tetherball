package tetherball.things

import tetherball.Tetherball.Thing
import skitch.vector.vec2
import skitch.core.managed.components.Position2D

object Pole extends Thing with Position2D {

	val position = vec2.zero
	val scale = vec2.one
	val rotation = 0.0

	def update(dt:Float) = ()

	def render() {

	}
}
