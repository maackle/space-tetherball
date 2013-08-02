package tetherball.things

import tetherball.TetherballGame.Thing
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
import tetherball.TetherballGame.Bits._
import scala.Some
import skitch.helpers.{Memory, OncePerFrame}
import skitch.Types.Radian
import tetherball.Winding


class Tether(numNodes:Int, val pole:Pole, val ball:Ball)(implicit world:World, app:SkitchApp) extends Thing with B2Implicits { rope =>

	val slackFactor = 1f

	private val direction = (ball.position - pole.position).unit

	object poleNode extends Node( pole.position + direction * (pole.radius + Node.radius), 0 ) {
		override val isTouching = true
	}

	object ballNode extends Node( ball.position - direction * (ball.radius + Node.radius), numNodes-1 )

	lazy val setup = {

		val numInnerNodes = numNodes - 2
		val innerNodes = for(i <- List.range(1, numInnerNodes+1)) yield {
			val t = i.toFloat / (numNodes-1)
			new Node( vec.lerp(poleNode.position, ballNode.position, t), i )
		}

		val poleWeld = Tether.JointFactory.weld(pole.body, poleNode.body, pole.position + direction * pole.radius)
		val ballWeld = Tether.JointFactory.weld(ball.body, ballNode.body, ball.position - direction * ball.radius)

		innerNodes
	}

	val innerNodes = setup

	val allNodes = (poleNode :: (ballNode :: innerNodes.reverse).reverse).toIndexedSeq

	val attachmentAngle = (poleNode.position - pole.position).angle

	val contactPoint = new ControlPoint(poleNode.position)
	contactPoint.body.setType(BodyType.KINEMATIC)

	val ballKinematicNode = new ControlPoint(ballNode.position)
	ballKinematicNode.body.setType(BodyType.KINEMATIC)

	val initialLength = (ballNode.position - poleNode.position).length
	val maxLength = initialLength * 1.1f
	val initialSpacing = initialLength / (numNodes - 1)

	val nodePairs = for((a,b) <- helpers.pairs(allNodes)) yield NodePair(a, b)

	val poleLimits = (innerNodes).map(new PoleLimit(_))
//	val tensionLimits = (innerNodes).map(new TensionLimit(_))
//	val ballLimits:Seq[HardLimit] = (innerNodes).map(new BallLimit(_))
	val grandLimiter = new GrandLimit

	val hardLimits = (
	Seq(grandLimiter)
	++: poleLimits
//	++: tensionLimits
//	++: ballLimits
	)

	lazy val font = new Font("font/UbuntuMono-R.ttf", 24)

	val memWindingAngle = helpers.MemFloat(300)
	val memRealSlackLength = helpers.MemFloat(300)
	val memIdealSlackLength = helpers.MemFloat(300)

	val interNodeWrappedAngle = {
		val R = Node.radius + pole.radius
		val L = initialSpacing
		math.acos( (2*R*R - L*L) / (2*R*R) )
	}

	val wrappedChain = OncePerFrame {
		allNodes.takeWhile { n =>
			n.isTouching
		}
	}

	val wrappedChainPairs = OncePerFrame {
		if(wrappedChain.isEmpty) IndexedSeq.empty
		else nodePairs.take(wrappedChain.last.ordinal)
	}

	def windingAngle:Radian = {
		ballNode.windAngle
	}

	def windingDirection = math.signum(windingAngle)

	def winding = Winding(windingDirection.toInt)

	def idealWoundLength:Real = {
		val r = pole.radius + Node.radius
		math.abs(contactAngle.toFloat * r)
	}

	def realWoundLength_sortOf:Real = {
		wrappedChainPairs.map( p => math.max(0, p.distance) ).sum
	}

	val realLength = OncePerFrame {
		(for( p <- nodePairs ) yield {
			p.distance()
		}).sum
	}

	def realSlackLength = realLength() - idealWoundLength

	def idealSlackLength = initialLength - idealWoundLength

	def error = {
		(for( p <- nodePairs ) yield {
			math.pow(math.max(p.distance() - initialSpacing, 0), 2).toFloat
		}).sum
	}

	def isLongerThanShouldBe = realSlackLength > idealSlackLength
//	def isTaut = grandLimiter.isActive
//	def isTaut = (ballNode.position - contactPoint.position).length >= grandLimiter.maxLength
	def isTautBy(extra:Float) = grandLimiter.isActive && realSlackLength > idealSlackLength * extra
	def isTaut = grandLimiter.isActive && isLongerThanShouldBe
	def isMerelyTight = grandLimiter.actualLength > tightRange._1

	def wrappedRatio = {
		idealWoundLength / realLength()
	}


	def contactAngle: Radian = {
		val wa = windingAngle
		val halfPi = math.Pi / 2 * 1.11f

		if (wa > halfPi) wa - halfPi
		else if (wa < -halfPi) wa + halfPi
		else 0.0
	}

	def tightRange = (grandLimiter.maxLength * 0.9f, grandLimiter.maxLength * 1.01f)

	def contactPointPosition: vec2 = vec.polar(pole.radius + Node.radius, contactAngle + attachmentAngle)

	def update(dt:Float) {
		contactPoint.body.setTransform(contactPointPosition, 0)
		ballKinematicNode.body.setTransform(ballNode.position, 0)
		allNodes.foreach(_.update(dt))
		hardLimits.foreach(_.update(dt))
		nodePairs.foreach(_.update(dt))

		memWindingAngle << windingAngle.toFloat
		memRealSlackLength  << realSlackLength.toFloat
		memIdealSlackLength  << idealSlackLength.toFloat
	}

	def render() {
		Color(0xff00ff).bind()
		gfx.circle(0.1f, contactPointPosition)

		if(false && rope.isTaut) Color.red.bind()
		else Color.white.bind()
		gl.lineWidth(2f)
		gl.begin(GL11.GL_LINE_STRIP) {
			allNodes.foreach(n => gl.vertex(n.renderPosition))
		}
		gl.lineWidth(1f)

		allNodes.foreach(_.render())

//		Color(0x555555).bind()
//		drawGraph(memWindingAngle)
//		Color(0x550055).bind()
//		drawGraph(memRealSlackLength)
//		Color(0x660022).bind()
//		drawGraph(memIdealSlackLength)
	}

	def drawGraph(mem:Memory[Float]) {
		gl.matrix {
			gl.scale(1/50f, 0.5f)
			gl.begin(GL11.GL_LINE_STRIP) {
				for ((y, t) <- mem.mem.zipWithIndex) {
					gl.vertex(t, y)
				}
			}
		}
	}

	object Node {

		val radius = 0.075f
		val density = 0.75f
	}

	class Node(initialPosition:vec2, val ordinal:Int)(implicit val world:World) extends ManagedEmbodied with CircleShape {

		val radius = Node.radius
		private[Tether] var _touching = false
		private[Tether] var _jointA, _jointB: Option[Joint] = None
		private[Tether] var _windAngle = 0.0

		def maxWindAngle = rope.interNodeWrappedAngle * ordinal

		def jointA = _jointA
		def jointB = _jointB
		def windAngle = _windAngle

		def setTouching(on:Boolean) {
			_touching = on
		}

		def isTouching = _touching

		// essentially, is this node "behind" the contactNode?  If so we don't have to "rein it in"
		def isBehindContactNode = {
			val sign = math.signum(rope.contactAngle)
			windAngle * sign < rope.contactAngle * sign
		}

		def projectedPosition = {
			val end2end = (ballNode.position - contactPoint.position)
			(position - contactPoint.position).project(end2end) + contactPoint.position
		}

		override def update(dt:Float) {
			if (isTouching) {
				val normal = (position - rope.pole.position).unit
				val tangent = normal.rotate(math.Pi/2 * windingDirection)
				if(velocity.dot(normal) < 0) {
					// remove velocity component going into the pole (to discourage from going *through* it)
					body.setLinearVelocity( velocity.project(tangent) )
				}
				if(math.abs(windAngle) > maxWindAngle && velocity.dot(tangent) > 0) {
					// if already wrapped too far, remove velocity component which would cause further wrapping
					body.setLinearVelocity( velocity.project(normal) )
				}
				// this used to be an attempt to modify the position directly -- totally unstable.
				if (isTouching && math.abs(windAngle) > maxWindAngle) {
					val ideal = vec.polar(Node.radius + rope.pole.radius, rope.windingDirection * maxWindAngle + poleNode.position.angle)

				}
			}
		}

		def renderPosition = {

				val (lo, hi) = tightRange
				val ratio = {
					(grandLimiter.actualLength() - lo) / (hi - lo)
				}
				if (ratio > 0) {
					if (isBehindContactNode) position
					else {
						val clampedRatio = math.max(0, math.min(1, ratio))
						vec.lerp(position, projectedPosition, clampedRatio)
					}
				}
				else position

		}

		def render() {
			if (isBehindContactNode && math.abs(windAngle) > maxWindAngle*0.9) {
				gl.fill(true)
				Color.cyan.bind()
			}
			else {
				gl.fill(false)
				Color.yellow.bind()
			}
			gfx.circle(radius, renderPosition)
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
			fixture.restitution = 1f
			fixture.density = Node.density
			fixture.userData = this
			fixture.filter = filter

			body.createFixture(fixture)
			body
		}
	}

	class ControlPoint(initialPosition:vec2)(implicit world:World) extends ManagedEmbodied {

		lazy val body = {
			val bodydef = Embodied.defaults.bodyDef

			bodydef.`type` = BodyType.KINEMATIC
			bodydef.position = initialPosition

			val body = world.createBody(bodydef)

			body
		}

		def render() {

		}
	}

	case class NodePair(a:Node, b:Node) {

		val pair = (a,b)

		val stick = Tether.JointFactory.stick(a, b, slackFactor)

//		val hinge = Tether.JointFactory.hinge(a, b)

		val distance = OncePerFrame {
			(b.position - a.position).length
		}

		def update(dt:Float) {
			val s = (a.position - pole.position).angle
			val t = (b.position - pole.position).angle
			b._windAngle = a._windAngle + helpers.Radian.diff(t, s)

			val dot = (a.velocity dot b.velocity)
			val diff = (b.position - a.position)
			val projA = a.velocity.project(diff)
			val projB = b.velocity.project(diff)
			// an attempt to never allow divergent velocity near the pole.  TODO: make it happen.
			if ((a.isTouching || b.isTouching) && diff.length > rope.initialSpacing) {
//				b.velocity = b.velocity - projB + projA
			}
		}

	}

	class GrandLimit extends PoleLimit(ballNode) {

		override val activeColor = Color.cyan
		override val minLength:Real = 0f
		override val baseFreq = 60f
		override val damping = 0.0f

		override def render() {
			gl.lineWidth(2f)
			super.render()
			gl.lineWidth(1f)
		}

	}

	class BallLimit(node:Node) extends HardLimit(ballKinematicNode, node) {

		val activeColor = Color.magenta

		val baseFreq = 5f
		val damping = 0f

		val minLength:Real = 0f

		val maxLength = {
			initialLength * 0.99f
		}

		def isActive = {
			val strictly = actualLength > minLength && ! node.isBehindContactNode && ! node.isTouching
			val also = (actualLength > maxLength && outwardSpeed > 0) || (actualLength > maxLength * 1.1f)
			strictly && also
		}
	}

	class PoleLimit(node:Node) extends HardLimit(contactPoint, node) {

		val activeColor = Color.blue

		val baseFreq = 5f
		val damping = 0f

		val minLength:Real = rope.pole.radius*4

		def maxLength = maxLengthCalc()

		val maxLengthCalc = {
			OncePerFrame {
//				val compensation = wrappedChainPairs.map( p => math.max(0, p.distance - rope.initialSpacing) ).sum
				val total = initialLength // + compensation
				math.max(0, total - rope.idealWoundLength)
			}
		}

		def isActive = {
			val strictly = actualLength > minLength && ! node.isBehindContactNode
			val also = (actualLength > maxLength && outwardSpeed > 0) || (actualLength > maxLength * 1.1f)
			strictly && also
		}
	}

	class TensionLimit(node:Node) extends {
		val control = new ControlPoint(node.position)
	} with HardLimit(control, node) {

		val activeColor = Color.yellow

		def baseFreq = 0f
		val damping = 0f

		val minLength = 0.0f
		val maxLength = 0f

		def individuallyActive = ! node.isBehindContactNode && ! node.isTouching && actualLength > minLength
		def isActive = (rope.isTaut) && individuallyActive

		override def update(dt:Float) {
			super.update(dt)
			control.body.setTransform(node.projectedPosition, 0f)
		}

		override def render() {
			super.render()
			if(individuallyActive) {
				Color.green.bind()
				gfx.circle(Node.radius/2, control.position)
			}
		}
	}

	abstract class HardLimit(val anchorNode:ControlPoint, val node:Node) {

		val earlyTriggerRatio = 0.95f
		val initialLength = (anchorNode.body.getPosition - node.body.getPosition).length
		def baseFreq:Float
		def damping:Float
		val eps = 1e-5f

		// the joint
		val j = {
			val jd = new DistanceJointDef
			jd.initialize(anchorNode.body, node.body, anchorNode.body.getPosition, node.body.getPosition)
			jd.length = initialLength
			jd.frequencyHz = Tether.JointFactory.freqHz
			jd.dampingRatio = damping
			world.createJoint(jd).asInstanceOf[DistanceJoint]
		}

		// speed in the direction pointing out from the anchor
		def outwardSpeed: Real = {
			( (node.position - anchorNode.position).unit dot (node.velocity) )
		}

		// real distance from anchor to node
		val actualLength = OncePerFrame {
			(node.position - anchorNode.position).length
		}

		// length below which the limit is never activated
		val minLength:Real

		// length at which the limit becomes active
		def maxLength:Real

		val disabledFreq = eps

		def activeFreq = {
			if(actualLength < minLength*2) baseFreq// * (actualLength / minLength*2)
			else baseFreq
		}

		def isActive:Boolean

		def update(dt:Float) {
			val diff = (node.position - anchorNode.position)
			val over = diff.length / maxLength
			if (over > 1) {
//				node.body.applyForceToCenter(impulse)
			}

			if (isActive) {
				j.setLength(maxLength)
				j.setFrequency(activeFreq)
			}
			else {
				j.setLength(maxLength * earlyTriggerRatio)
				j.setFrequency(disabledFreq)
			}
		}

		def activeColor:Color
		val disabledColor = Color.gray.alpha(0)

		def render() {
			val diff = (node.position - anchorNode.position)

			if (isActive) activeColor.bind()
			else disabledColor.bind()
			gfx.vector(anchorNode.body.getPosition, diff.unit * maxLength)
		}
	}

}

object Tether {

	object JointFactory extends B2Implicits {
		val freqHz = 0f
		val damping = 0f

		def stick(a:Tether#Node, b:Tether#Node, slackFactor:Float)(implicit world:World):DistanceJoint = {

			val jd = new DistanceJointDef
			jd.initialize(a.body, b.body, a.body.getPosition, b.body.getPosition)
			jd.length = (b.position - a.position).length * slackFactor
			jd.frequencyHz = JointFactory.freqHz
			jd.dampingRatio = JointFactory.damping

			val j = world.createJoint(jd).asInstanceOf[DistanceJoint]
			a._jointB = Some(j)
			b._jointA = Some(j)
			j
		}

		def hinge(a:Tether#Node, b:Tether#Node)(implicit world:World):RevoluteJoint = {

			val jd = new RevoluteJointDef
			jd.initialize(a.body, b.body, (a.position + b.position) / 2)
			jd.referenceAngle = math.Pi.toFloat
			jd.enableMotor = false
			jd.lowerAngle = math.Pi.toFloat * 1 / 2
			jd.upperAngle = math.Pi.toFloat * 3 / 2
			val j = world.createJoint(jd).asInstanceOf[RevoluteJoint]
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
