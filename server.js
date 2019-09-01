const child_process = require('child_process')
const express = require('express')
const fs = require('fs')
const path = require('path')

const app = express()
const port = 3000
const dir = './results/'
const extension = '.data'

const benchmarks = [
  'Bounce', 'CD', 'DeltaBlue', 'Havlak', 'Json', 'List', 'Mandelbrot', 'NBody',
  'Permute', 'Queens', 'Richards', 'Sieve', 'Storage', 'Towers', ]

const stats = {
  run1: { max: 0, min: 0, mean: 0, },
  run2: { max: 0, min: 0, mean: 0, },
}

console.log(`Initializing server...`)

Polyglot.export('stats', stats)

Polyglot.evalFile('R', 'plotting.R')
const example_plot = Polyglot.eval('R', 'example_plot')
const single_plot = Polyglot.eval('R', 'single_plot')
const diff_plot = Polyglot.eval('R', 'diff_plot')
const diff_summary_plot = Polyglot.eval('R', 'diff_summary_plot')
const summary_plot = Polyglot.eval('R', 'summary_plot')

const round = (x) => Math.round(x * 1000) / 1000
const shortCommit = (x) => x.substr(0, 8)

const extractNumber = (value) => {
  parsed = Number(value)
  return isNaN(parsed) ? 0 : parsed;
}

const gitCommitDate = (commit) => {
  let res = child_process.spawnSync('git',
    ['show', '-s', '--format=%ct', commit],
    { cwd: '/Users/fniephaus/dev/graal/graalsqueak'})
  if (res.error) {
    return res.err
  } else {
    return new Date(Number(res.stdout.toString()) * 1000).toLocaleString()
  }
}

app.set('view engine', 'pug')

app.use('/static', express.static('static'))

app.get('/', (req, res) => {
  let files
  try {
    files = fs.readdirSync(dir).
      filter(file => file.endsWith(extension)).
      map(file => file.split('.')[0]);
  } catch(err) {
    console.error(err)
  }

  res.render('index', { files })
})

app.get('/:commit([a-f0-9]+)/:startIteration(\\d+)?', (req, res) => {
  let commit = req.params.commit
  let file = path.join(dir, commit + extension)
  if (!fs.lstatSync(file).isFile()) {
    res.status(404).send('')
    return
  }
  let commitDate = gitCommitDate(commit)
  let startIteration = extractNumber(req.params.startIteration)
  let plot = summary_plot(file, startIteration)
  res.render('benchmark_summary', {
    commit,
    commitDate,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.get('/:commit1([a-f0-9]+)/:commit2([a-f0-9]+)/:startIteration(\\d+)?', (req, res) => {
  let commit1 = req.params.commit1
  let commit2 = req.params.commit2
  let commit1Short = shortCommit(commit1)
  let commit2Short = shortCommit(commit2)
  let commitDate1 = gitCommitDate(commit1)
  let commitDate2 = gitCommitDate(commit2)
  let file1 = path.join(dir, commit1 + extension)
  let file2 = path.join(dir, commit2 + extension)
  if (!fs.lstatSync(file1).isFile() || !fs.lstatSync(file2).isFile()) {
    res.status(404).send('')
    return
  }

  let startIteration = extractNumber(req.params.startIteration)
  let plot = diff_summary_plot(commit1Short, file1, commit2Short, file2, startIteration)
  res.render('benchmark_diff_summary', {
    commit1,
    commit2,
    commit1Short,
    commit2Short,
    commitDate1,
    commitDate2,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.get('/:commit([a-f0-9]+)/:benchmark(\\w+)/:startIteration(\\d+)?', (req, res) => {
  let commit = req.params.commit
  let file = path.join(dir, commit + extension)
  if (!fs.lstatSync(file).isFile()) {
    res.status(404).send('')
    return
  }
  let commitDate = gitCommitDate(commit)
  let benchmark = req.params.benchmark
  let startIteration = extractNumber(req.params.startIteration)
  let plot = single_plot(file, benchmark, startIteration)
  res.render('benchmark', {
    commit,
    commitDate,
    benchmark,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.get('/:commit1([a-f0-9]+)/:commit2([a-f0-9]+)/:benchmark(\\w+)/:startIteration(\\d+)?', (req, res) => {
  let commit1 = req.params.commit1
  let commit2 = req.params.commit2
  let commitDate1 = gitCommitDate(commit1)
  let commitDate2 = gitCommitDate(commit2)
  let file1 = path.join(dir, commit1 + extension)
  let file2 = path.join(dir, commit2 + extension)
  if (!fs.lstatSync(file1).isFile() || !fs.lstatSync(file2).isFile()) {
    res.status(404).send('')
    return
  }

  let benchmark = req.params.benchmark
  let startIteration = extractNumber(req.params.startIteration)
  let plot = diff_plot(file1, file2, benchmark, startIteration)
  res.render('benchmark_diff', {
    commit1,
    commit2,
    commitDate1,
    commitDate2,
    benchmark,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.listen(port, () => console.log(`Server listening on port ${port}...`))
