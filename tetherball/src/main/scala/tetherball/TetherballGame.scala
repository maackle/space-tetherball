package tetherball

import skitch.core.{Update, ResourceLoader, SkitchApp}
import tetherball.states.PlayState
import java.io.File
import grizzled.slf4j.Logging
import org.lwjgl.input.Keyboard
import skitch.audio.SoundSystem

object TetherballGame extends SkitchApp with App with Logging { self =>

	def initialize = {
		info("app starting")
		SoundSystem.initialize()
		loader.autoload()
	}

	def cleanup = {
		info("app closing")
		SoundSystem.destroy()
	}

	val loader = new ResourceLoader(new File("tetherball/src/main/resources"))
	val initialWindowSize = Some(1600, 1200)
	val windowTitle = "space tetherball"
	lazy val startState = new PlayState
	val worldScale = 1f / 30f
	val fps = 60

	object CollisionBits {
		val POLE_BIT = 0x01
		val ROPE_NODE_BIT = 0x02
		val BALL_BIT = 0x04
		val PLAYER_BIT = 0x08
	}

	abstract class Thing extends skitch.core.managed.Thing

	run()
}

object Winding extends Enumeration {
	val CW = Value(-1)
	val CCW = Value(1)
}

class Countdown(seconds:Float)(onTimeout: =>Unit) extends Update {
	private var t = 0f

	def update(dt:Float) {
		if (isRunning) {
			t -= dt
			if (t <= 0) {
				onTimeout
			}
		}
		else {
			t = 0
		}
	}

	def isRunning = t > 0

	def elapsed = seconds - t

	def completion = {
		1 - t / seconds
	}

	def start() {
		if( ! isRunning ) {
			t = seconds
		}
	}
}
