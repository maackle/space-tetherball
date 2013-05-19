package tetherball.things

import tetherball.Tetherball.Thing
import skitch.stage.box2d.{Embodied, B2Implicits, ManagedEmbodied}
import skitch.vector.{vec, vec2}
import org.jbox2d.dynamics.{Body, Filter, BodyType, World}
import skitch._
import skitch.core.components.CircleShape
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.joints._
import skitch.helpers
import skitch.gfx
import Types._
import skitch.gfx.Font
import skitch.core.SkitchApp
import org.lwjgl.opengl.GL11
import tetherball.Tetherball
import tetherball.Tetherball.Bits._
import scala.Some

class Rope(numNodes:Int, pole:Pole, val ball:Ball)(implicit world:World, app:SkitchApp) extends Thing with B2Implicits { rope =>

	import Rope._

	val slackFactor = 1f

	val initialLength = (ball.position - pole.position).length
	val initialSpacing = initialLength / (numNodes - 1)

	lazy val setup = {
		val pole2ball = ball.position - pole.position
		val direction = pole2ball.unit

		val poleNode = new Node( pole.position + direction * (pole.radius + Rope.radius) )
		val ballNode = new Node( ball.position - direction * (ball.radius + Rope.radius) )

		val numInnerNodes = numNodes - 2
		val innerNodes = for(i <- List.range(1, numInnerNodes)) yield {
			val t = i.toFloat / numInnerNodes
			new Node( vec.lerp(poleNode.position, ballNode.position, t) )
		}

		val allNodes = (poleNode :: (ballNode :: innerNodes.reverse).reverse)

		val stickJoints = for( (a,b) <- helpers.pairs(allNodes.take(allNodes.size-0))) yield {
			Rope.Joint.stick(a, b, slackFactor)
		}

		val poleWeld = Rope.Joint.weld(pole.body, poleNode.body, pole.position + direction * pole.radius)
		val ballWeld = Rope.Joint.weld(ball.body, ballNode.body, ball.position - direction * ball.radius)

		val attachmentAngle = (poleNode.position - pole.position).angle

		// a moving dummy node from which all `HardLimit`s are based
		val contactNode = new Node(poleNode.position)
		contactNode.body.setType(BodyType.KINEMATIC)

		(poleNode, ballNode, innerNodes, allNodes, contactNode, attachmentAngle)
	}

	lazy val (poleNode, ballNode, innerNodes, allNodes, contactNode, attachmentAngle) = setup

	val hardLimits = (ballNode :: innerNodes).map(new HardLimit(_))

	lazy val font = new Font("font/UbuntuMono-R.ttf", 24)

	val history = helpers.MemFloat(500)

	def windingAngle:Radian = {
		ballNode.windAngle
	}

	def woundLength:Real = {
		val r = pole.radius + Rope.radius
		math.abs(contactAngle.toFloat * r)
	}

	def contactAngle: Radian = {
		val wa = windingAngle
		val halfPi = math.Pi / 2

		if (wa > halfPi) wa - halfPi
		else if (wa < -halfPi) wa + halfPi
		else 0.0
	}

	def contactPoint: vec2 = vec.polar(pole.radius + Rope.radius, contactAngle + attachmentAngle)

	def update(dt:Float) {
		contactNode.body.setTransform(contactPoint, 0)
		allNodes.foreach(_.update(dt))
		hardLimits.foreach(_.update(dt))

		helpers.pairs(allNodes).foreach({ p=>
			val (a,b) = p
			val s = (a.position - pole.position).angle
			val t = (b.position - pole.position).angle
			b._windAngle = a._windAngle + helpers.Radian.diff(t, s)
		})
	}

	def render() {
		Color(0xff00ff).bind()
		gfx.circle(0.1f, contactPoint)
		allNodes.foreach(_.render())
		hardLimits.foreach(_.render())

		history << windingAngle.toFloat

		gl.matrix {
			gl.scale(1/50f, 0.5f)
			gl.begin(GL11.GL_LINE_STRIP) {
				for ((y, t) <- history.mem.zipWithIndex) {
					gl.vertex(t, y + 2)
				}
			}
		}
	}

	class Node(initialPosition:vec2)(implicit val world:World) extends ManagedEmbodied with CircleShape {

		val radius = Rope.radius
		private[Rope] var isTouching = false
		private[Rope] var _jointA, _jointB: Option[DistanceJoint] = None
		private[Rope] var _windAngle = 0.0

		def jointA = _jointA
		def jointB = _jointB
		def windAngle = _windAngle

		def setTouching(on:Boolean) {
			isTouching = on

			if (isTouching) {
				jointA.map { j =>
					j.setFrequency(Joint.freqHzTight)
				}
			}
			else {
				jointA.map { j =>
					j.setFrequency(Joint.freqHz)
				}
			}
		}

		def isWoundUp = {
			val sign = math.signum(rope.contactAngle)
			windAngle * sign < rope.contactAngle * sign
		}

		override def update(dt:Float) {
			if (isTouching) {
				val normal = (position - rope.pole.position).unit
				if(velocity.dot(normal) < 0) {
					body.setLinearVelocity( velocity.project(normal.rotate(math.Pi/2)) )
				}
			}
		}

		def render() {
			if (isWoundUp) {
				gl.fill(true)
				Color.cyan.bind()
			}
			else {
				gl.fill(false)
				Color.yellow.bind()
			}
			gfx.circle(radius, position)

		}

		lazy val body = {
			val fixture = Embodied.defaults.fixtureDef
			val bodydef = Embodied.defaults.bodyDef

			bodydef.`type` = BodyType.DYNAMIC
			bodydef.position = initialPosition
			val body = world.createBody(bodydef)

			val filter = new Filter
			filter.categoryBits = ROPE_NODE_BIT
			filter.maskBits = POLE_BIT

			val circle = new shapes.CircleShape()
			circle.m_radius = this.radius

			fixture.shape = circle
			fixture.restitution = 0f
			fixture.density = 1f
			fixture.userData = this
			fixture.filter = filter

			body.createFixture(fixture)
			body
		}
	}


	class HardLimit(val node:Node) {

		val earlyTriggerRatio = 0.95f
		val initialLength = (contactNode.body.getPosition - node.body.getPosition).length * earlyTriggerRatio
		val baseFreq = 30f
		val eps = 1e-5f

		val j = {
			val jd = new DistanceJointDef
			jd.initialize(contactNode.body, node.body, contactNode.body.getPosition, node.body.getPosition)
			jd.length = initialLength
			jd.frequencyHz = Rope.Joint.freqHz
			jd.dampingRatio = Rope.Joint.damping
			world.createJoint(jd).asInstanceOf[DistanceJoint]
		}

		def outwardSpeed: Real = {
			( (node.position - contactNode.position).unit dot (node.velocity) )
		}

		def actualLength:Real = (node.position - contactNode.position).length

		val minLength:Real = rope.pole.radius * 2

		def maxLength:Real = {
			math.max(0, initialLength - rope.woundLength)
		}

		val disabledFreq = eps

		def activeFreq = baseFreq * (1 + 5*(initialLength - actualLength)/initialLength)

		def active = {
			actualLength > minLength && actualLength / maxLength > 1 && ! node.isWoundUp && outwardSpeed > 0
		}

		def update(dt:Float) {
			val diff = (node.position - contactNode.position)
			val over = diff.length / maxLength
			val impulse = diff.unit * (-10000*dt * over)
			if (over > 1) {
//				node.body.applyForceToCenter(impulse)
			}


			if (active) {
				j.setLength(maxLength)
				j.setFrequency(activeFreq)
			}
			else {
				j.setLength(maxLength * earlyTriggerRatio)
				j.setFrequency(disabledFreq)
			}
		}

		def render() {
			val diff = (node.position - contactNode.position)

			if (j.getFrequency > eps) Color.yellow.bind()
			else Color.gray.bind()
			gfx.vector(contactNode.body.getPosition, diff.unit * maxLength)

		}
	}

}

object Rope {

	val radius = 0.075f

	object Joint extends B2Implicits {
		val freqHz = 30f
		val freqHzTight = freqHz * 2
		val damping = 0.5f

		def stick(a:Rope#Node, b:Rope#Node, slackFactor:Float)(implicit world:World):DistanceJoint = {

			val jd = new DistanceJointDef
			jd.initialize(a.body, b.body, a.body.getPosition, b.body.getPosition)
			jd.length = (b.position - a.position).length * slackFactor
			jd.frequencyHz = Joint.freqHz
			jd.dampingRatio = Joint.damping

			val j = world.createJoint(jd).asInstanceOf[DistanceJoint]
			a._jointB = Some(j)
			b._jointA = Some(j)
			j
		}

		def weld(a:Body, b:Body, pos:vec2)(implicit world:World) = {
			val jw = new WeldJointDef
			jw.initialize(a, b, pos)

			world.createJoint(jw)
		}
	}

}
