const express = require('express')
const fs = require('fs')
const path = require('path')
const process = require('process')

const app = express()
const port = 3000
const resultsDir = path.join(process.cwd(), '/results')
const fileRegex = ':year(\\d\\d\\d\\d)-:month(\\d\\d)-:day(\\d\\d).:time([0-9:]+)-:commit([a-f0-9]+)-:branch.data'
const file1Regex = ':year1(\\d\\d\\d\\d)-:month1(\\d\\d)-:day1(\\d\\d).:time1([0-9:]+)-:commit1([a-f0-9]+)-:branch1.data'
const file2Regex = ':year2(\\d\\d\\d\\d)-:month2(\\d\\d)-:day2(\\d\\d).:time2([0-9:]+)-:commit2([a-f0-9]+)-:branch2.data'
const extension = '.data'

const benchmarks = [
  'Bounce', 'CD', 'DeltaBlue', 'Havlak', 'Json', 'List', 'Mandelbrot', 'NBody',
  'Permute', 'Queens', 'Richards', 'Sieve', 'Storage', 'Towers', ]

const stats = {
  run1: { min: 0, geomean: 0, median: 0, mean: 0, max: 0, },
  run2: { min: 0, geomean: 0, median: 0, mean: 0, max: 0, },
}

console.log(`Initializing server...`)

Polyglot.export('stats', stats)

Polyglot.evalFile('R', 'src/plotting.R')
const benchmark_plot = Polyglot.eval('R', 'benchmark_plot')
const benchmark_diff_plot = Polyglot.eval('R', 'benchmark_diff_plot')
const summary_plot = Polyglot.eval('R', 'summary_plot')
const summary_diff_plot = Polyglot.eval('R', 'summary_diff_plot')

const round = (x) => Math.round(x * 1000) / 1000
const shortCommit = (x) => x.substr(0, 8)
const extractDate = (fileName) => fileName.substr(0, 19)

const extractNumber = (value) => {
  parsed = Number(value)
  return isNaN(parsed) ? 0 : parsed;
}

app.set('view engine', 'pug')

app.use('/static', express.static('static'))

app.get('/', (req, res) => {
  let files
  try {
    files = fs.readdirSync(resultsDir).
      filter(file => file.endsWith(extension) && file.split('-').length >= 5);
  } catch(err) {
    console.error(err)
  }

  res.render('index', { files })
})

app.get('/' + fileRegex + '/:startIteration(\\d+)?', (req, res) => {
  let file = req.originalUrl.split('/')[1]
  let filePath = path.join(resultsDir, file)
  if (!fs.lstatSync(filePath).isFile()) {
    res.status(404).send('')
    return
  }
  let commit = req.params.commit
  let commitDate = extractDate(file)
  let commitBranch = req.params.branch
  let startIteration = extractNumber(req.params.startIteration)
  let plot = summary_plot(filePath, startIteration)
  res.render('summary', {
    file,
    commit,
    commitDate,
    commitBranch,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.get('/' + file1Regex + '/' + file2Regex + '/:startIteration(\\d+)?', (req, res) => {
  let file1 = req.originalUrl.split('/')[1]
  let file2 = req.originalUrl.split('/')[2]
  let file1Path = path.join(resultsDir, file1)
  let file2Path = path.join(resultsDir, file2)
  if (!fs.lstatSync(file1Path).isFile() || !fs.lstatSync(file2Path).isFile()) {
    res.status(404).send('')
    return
  }
  let commit1 = req.params.commit1
  let commit2 = req.params.commit2
  let commit1Short = shortCommit(commit1)
  let commit2Short = shortCommit(commit2)
  let commit1Date = extractDate(file1)
  let commit2Date = extractDate(file2)
  let commit1Branch = req.params.branch1
  let commit2Branch = req.params.branch2
  let startIteration = extractNumber(req.params.startIteration)
  let plot = summary_diff_plot(
    commit1Short, commit1Date, file1Path,
    commit2Short, commit2Date, file2Path,
    startIteration)
  res.render('summary_diff', {
    file1, file2,
    commit1, commit2,
    commit1Short, commit2Short,
    commit1Date, commit2Date,
    commit1Branch, commit2Branch,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.get('/' + fileRegex + '/:benchmark(\\w+)/:startIteration(\\d+)?', (req, res) => {
  let file = req.originalUrl.split('/')[1];
  let filePath = path.join(resultsDir, file)
  if (!fs.lstatSync(filePath).isFile()) {
    res.status(404).send('')
    return
  }
  let commit = req.params.commit
  let commitDate = extractDate(file)
  let commitBranch = req.params.branch
  let benchmark = req.params.benchmark
  let startIteration = extractNumber(req.params.startIteration)
  let plot = benchmark_plot(filePath, benchmark, startIteration)
  res.render('benchmark', {
    file,
    commit,
    commitDate,
    commitBranch,
    benchmark,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.get('/' + file1Regex + '/' + file2Regex + '/:benchmark(\\w+)/:startIteration(\\d+)?', (req, res) => {
  let file1 = req.originalUrl.split('/')[1]
  let file2 = req.originalUrl.split('/')[2]
  let file1Path = path.join(resultsDir, file1)
  let file2Path = path.join(resultsDir, file2)
  if (!fs.lstatSync(file1Path).isFile() || !fs.lstatSync(file2Path).isFile()) {
    res.status(404).send('')
    return
  }

  let commit1 = req.params.commit1
  let commit2 = req.params.commit2
  let commit1Date = extractDate(file1)
  let commit2Date = extractDate(file2)
  let commit1Branch = req.params.branch1
  let commit2Branch = req.params.branch2
  let benchmark = req.params.benchmark
  let startIteration = extractNumber(req.params.startIteration)
  let plot = benchmark_diff_plot(file1Path, file2Path, benchmark, startIteration)
  res.render('benchmark_diff', {
    file1, file2,
    commit1, commit2,
    commit1Date, commit2Date,
    commit1Branch, commit2Branch,
    benchmark,
    startIteration,
    benchmarks,
    round,
    plot, req, stats })
})

app.listen(port, () => console.log(`Server listening on port ${port}...`))
