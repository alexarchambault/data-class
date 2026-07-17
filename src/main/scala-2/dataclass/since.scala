package dataclass

import scala.annotation.StaticAnnotation

class since(val version: String = "") extends StaticAnnotation
