package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day07 = Day(7) { (input, part) =>

  enum Statement:
    case ChangeDirectory(directory: String)
    case ListContent
    case File(name: String, size: Int)
    case Directory(name: String)
  import Statement.*

  val statements: Seq[Statement] = input.split(lineSeparator).toSeq.map {
    case s"$$ $command" => command match
      case s"cd $directory" => ChangeDirectory(directory)
      case s"ls" => ListContent
    case s"dir $directory" => Directory(directory)
    case s"$size $name" => File(name, size.toInt)
  }

  type Content = Map[Seq[String], Set[File | Directory]]

  val (_, content) = statements.foldLeft((Seq.empty[String], Map.empty: Content)) { case ((path, content), head) =>
    head match
      case ChangeDirectory(directory) => directory match
        case ".." => (path.init, content)
        case _ => (path :+ directory, content)
      case ListContent => (path, content)
      case file: File => (path, content + (path -> (content.getOrElse(path, Set.empty) + file)))
      case directory: Directory => (path, content + (path -> (content.getOrElse(path, Set.empty) + directory)))
  }
  val topological = content.keySet.toSeq.sortBy(-_.size)
  val sizes = topological.foldLeft(Map.empty[Seq[String], Int]) { case (map, e) =>
    map + (e -> content(e).map {
      case File(_, size) => size
      case Directory(name) => map(e :+ name)
    }.sum)
  }

  part(1) = sizes.values.filter(_ <= 100000).sum

  val total = sizes(topological.last)

  part(2) = sizes.values.filter(total - _ <= 40000000).toSeq.min

}
