package tetherball.states

import skitch.core.{KeyDown, SkitchState}
import skitch.{gfx, Color, gl}
import skitch.vector.vec2

class PauseState(parent:PlayState) extends SkitchState(parent.app) {

	def update(dt:Float) {

	}

	def render() {

	}

	def onEnter {
		parent.render()
		Color(0, 0, 0, 0.5f).bind()
		gl.fill(true)
		gfx.rect(app.projectionRect)
	}

	def onExit {

	}

	listen {
		case KeyDown(KEY_SPACE) => app.popState()
	}
}
