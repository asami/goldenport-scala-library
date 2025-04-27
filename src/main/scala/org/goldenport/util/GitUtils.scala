package org.goldenport.util

import collection.JavaConverters._
import java.io.File
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.GitAPIException
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.lib.Repository

/*
 * @since   Apr. 26, 2025
 * @version Apr. 26, 2025
 * @author  ASAMI, Tomoharu
 */
object GitUtils {
  def initAndCommit(projdir: File): Unit = {
    val repo = openRepository(projdir)
    try {
      val git = makeInitializedGit(projdir, repo)
      try {
        addAndCommitAllSources(git)
      } finally {
        git.close()
      }
    } finally {
      repo.close()
    }
  }

  def openRepository(projdir: File): Repository = {
    val gitdir = new File(projdir, ".git")
    new FileRepositoryBuilder()
    .setGitDir(gitdir)
    .setWorkTree(projdir)
    .readEnvironment()
    .findGitDir()
    .build()
  }

  def makeInitializedGit(projdir: File, repo: Repository): Git = {
    val gitdir = repo.getDirectory
    if (!gitdir.exists()) {
      Git.init().setDirectory(projdir).call()
    } else {
      new Git(repo)
    } 
  }

  def addAndCommitAllSources(git: Git): Unit = {
    git.add().addFilepattern(".").call()
    git.commit()
      .setMessage("Initial commit")
      .setAllowEmpty(true) // Skip commit if nothing new
      .call()
  }
}
