package adventofcode.y2024

import scala.annotation.tailrec

object Day9 extends Year2024 {
  override val day = 9

  override def runDay(input: String): Unit = {
    val diskMap = input.map(_.asDigit).toSeq
    val (_, files, freeSpace) = diskMap.grouped(2).zipWithIndex.foldLeft((0, List.empty[File], List.empty[FreeSpace])) {
      case ((location, files, free), (pair, id)) =>
        pair match {
          case Seq(fileSize, freeSize) => (
            location + fileSize + freeSize,
            File(id, fileSize, location) :: files,
            FreeSpace(freeSize, location + fileSize) :: free
          )
          case Seq(fileSize) => (
            location + fileSize,
            File(id, fileSize, location) :: files,
            free
          )
        }
    }

    val compacted = compactSplitFiles(files, freeSpace)
    printDayPart(1, compacted.map(_.checksum).sum, "Checksum of compacted disk: %s")

    val compactedWhileFiles = compactWholeFiles(files, freeSpace)
    printDayPart(2, compactedWhileFiles.map(_.checksum).sum, "Checksum of compacted disk only moving whole files: %s")
  }

  private def compactSplitFiles(files: List[File], freeSpace: List[FreeSpace]): List[File] = {
    @tailrec
    def compact(files: List[File], freeSpace: List[FreeSpace], movedFiles: List[File]): List[File] =
      freeSpace match {
        case Nil => throw new IllegalStateException("No more free space")
        case free :: remainingFree =>
          val file :: otherFiles = files: @unchecked
          if (free.location > file.location) {
            movedFiles ::: files
          } else if (free.size >= file.size) {
            // println(s"Moving file $file to location ${free.location}")
            compact(
              otherFiles,
              if (free.size == file.size) remainingFree else freeSpace.updated(0, FreeSpace(free.size - file.size, free.location + file.size)),
              file.copy(location = free.location) :: movedFiles
            )
          } else {
            val movedPart = file.copy(size = free.size, location = free.location)
            val otherPart = file.copy(size = file.size - free.size)
            // println(s"Splitting file $file into $movedPart and $otherPart")
            compact(otherPart :: otherFiles, remainingFree, movedPart :: movedFiles)
          }
      }

    compact(files, freeSpace.reverse, List.empty)
  }

  private def compactWholeFiles(files: List[File], freeSpace: List[FreeSpace]): List[File] = {
    @tailrec
    def compact(files: List[File], freeSpace: List[FreeSpace], newFiles: List[File]): List[File] =
      files match {
        case Nil => newFiles
        case file :: otherFiles =>
          freeSpace.indexWhere(_.size >= file.size) match {
            case -1 =>
              compact(otherFiles, freeSpace, file :: newFiles)
            case freeSpaceIdx =>
              val free = freeSpace(freeSpaceIdx)
              if (free.location < file.location) {
                // println(s"Moving file $file to location ${free.location}")
                compact(
                  otherFiles,
                  freeSpace.updated(freeSpaceIdx, FreeSpace(free.size - file.size, free.location + file.size)),
                  file.copy(location = free.location) :: newFiles
                )
              } else {
                compact(otherFiles, freeSpace, file :: newFiles)
              }
          }
      }

    compact(files, freeSpace.reverse, List.empty)
  }

  private case class File(id: Int, size: Int, location: Int) {
    def checksum: Long = (location until location + size).map(_.toLong * id).sum
  }

  private case class FreeSpace(size: Int, location: Int)
}
