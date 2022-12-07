package adventofcode.y2022

import scala.io.BufferedSource

object Day7 extends Year2022 {
  override val day = 7

  private val CdCommandRE = """\$ cd (\w+|/|..)""".r
  private val LsCommandRE = """\$ ls""".r
  private val DirEntryRE = """dir (\w+)""".r
  private val FileEntryRE = """(\d+) ([\w.]+)""".r

  private val DISK_SIZE = 70_000_000
  private val SPACE_NEEDED = 30_000_000

  override def runDay(input: BufferedSource): Unit = {
    val root = input.getLines().foldLeft(List.empty[String], Dir("/")) { (state, line) =>
      val (path, root) = state
      line match {
        case CdCommandRE(name) if name == "/" => (List.empty, root)
        case CdCommandRE(name) if name == ".." => (path.drop(1), root)
        case CdCommandRE(name) => (name :: path, root)
        case LsCommandRE() => (path, root) // ignore
        case FileEntryRE(size, name) => (path, root.addFile(path.reverse, File(name, size.toInt)))
        case DirEntryRE(_) => (path, root) // ignore
      }
    }._2

    val allDirs = findDirsIn(root)
    val dirSizes = allDirs.map(_.size)
    printDayPart(1, dirSizes.filter(_ <= 100000).sum, "The sum of size of small folders: %s")

    val neededToDelete = SPACE_NEEDED - (DISK_SIZE - root.size)
    val possibleDirsToDelete = dirSizes.filter(_ > neededToDelete).sorted
    printDayPart(2, possibleDirsToDelete.head, "The size of the smallest folder to delete is: %s")
  }

  private def findDirsIn(dir: Dir): List[Dir] = {
    val subdirs = dir.children.filter(_.isInstanceOf[Dir]).map(_.asInstanceOf[Dir])
    subdirs ::: subdirs.flatMap(findDirsIn)
  }

  private sealed trait FsEntry {
    val name: String
    def size: Int
  }
  private case class File(name: String, size: Int) extends FsEntry
  private case class Dir(name: String, children: List[FsEntry] = List()) extends FsEntry {
    override def size: Int = children.map(_.size).sum
    def addFile(path: List[String], file: File): Dir = {
      if (path.isEmpty) {
        copy(children = file :: children)
      } else {
        val child = children.find(_.name == path.head).getOrElse(Dir(path.head)).asInstanceOf[Dir]
        copy(children = child.addFile(path.tail, file) :: children.filterNot(_.name == path.head))
      }
    }
  }
}
