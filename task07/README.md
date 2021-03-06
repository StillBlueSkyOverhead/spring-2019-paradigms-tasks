# Общее
Это домашнее задание состоит из множества независимых задач, в которых
вам требуется самостоятельно выполнить арифметические упражнения.
Все задачи оцениваются автоматически и проверяется только корректность
Все задачи сдаются через систему pull requests на GitHub.

Как обычно, вы можете задавать нам вопросы по задачам и решениям,
а также если автоматическая система проверки делает что-то странное.

# Подробнее
* Каждая задача оценивается независимо от остальных и стоит 0.5 балла
* Задачи сгруппированы по темам, темы сгруппированы в файлы с именами вида `??-*.md`
  Каждая тема вынесена в отдельный раздел и имеет двухбуквенный код
  Каждая задача однозначно характеризуется темой и своим номером внутри темы (например, `WU1`)
* Если в задаче не указано иное, используйте десятичную систему счисления и не используйте префиксы вроде `0b`, `0x`
* Специальные значения IEEE-754 записываются как `+inf`, `-inf` и `nan` (без учёта регистра)
* Если ответа не существует, напишите `n/a` (без учёта регистра)
* Для решения задачи вам требуется изменить сооветствующий `*.md` файл
  * В задачах с таблицами вам требуется заполнить все пустующие клетки таблицы
    * Не требуется аккуратно выравнивать исходный код таблицы или добавлять пробелы, главное — сохранить структуру
    * По желанию вы можете добавлять произвольные пробелы внутри ответов или оборачивать их в ```` `` ```` для более красивого вывода
  * В задачах `FL3` и `FL4` вам требуется изменить текст, написанный на отдельных строчках
    внутри ```` ``` ````.
  * Не меняйте в файле ничего, кроме ячеек таблиц или текста внутри ```` ``` ````

## Тесты
В этом задании автоматические тесты не требуются.

## Требования к корректности решения
Если присланные файлы не соответствуют хотя бы одному из условий ниже, решение считается некорректным и может быть оценено в 0 баллов:

* Решения должны быть внесены прямо в файлы `??-*.md`
* Должны быть оставлены без изменений: заголовки разделов и таблиц, нумерация задач

# Формат сдачи
Если возникают **любые** непонятки с форматом сдачи (даже простые и мелкие) - пишите преподавателю!
Мы не хотим, чтобы вы не сдали просто потому что кто-то не очень хорошо рассказал Git или дал мало практики :)

* Вы работаете в своём собственном fork'е этого репозитория на GitHub
  * Подробный пример работы с Git есть в описании задания `task06`
* Для сдачи домашнего задания откройте pull request в ветку `master` нашего репозитория
* Pull request должен удовлетворять следующим требованиям, иначе он не будет проверен:
  * Название: `Task07, GroupXX, Фамилия Имя`.
    * Например: `Task07, Group09, Суворов Егор`
    * Не допускаются сокращения, перестановка слов, отсутствие запятой, пробелов или написание имени транслитом
  * Отсутствуют merge conflicts
  * В сумме за все коммиты (вкладка "Files Changed") отсутствуют временные файлы вроде `.*.swp`, настройки среды вроде `.idea` и прочие файлы,
    которых не было в репозитории
* Вопросы следует задавать либо по почте, либо открыв Issue в репозитории в произвольной форме и с названием содержащим вопрос, начинающимся с `Task 07:`
* Для сдачи задания отправлять письмо не требуется, достаточно открытия Pull Request по установленной форме
* Проверка производится автоматически: после открытия pull request в течение примерно минуты приходит `@yeputons-bot`,
  назначает себя reviewer, отправляет вам review прямо на GitHub и обновляет таблицу с результатами
  * Approved означает полное засчитывание всех задач, которые вы попытались сдать в pull request
  * Changes requested означает, что что-то надо доделать (при этом могут быть уже зачтены какие-то задачи)
  * По каждой задаче, как обычно, выбирается лучшая попытка сдачи
  * Автоматическая проверка станет доступна до 22:59 30.03.2019 (суббота), но вы можете открыть pull request раньше
* Если вам кажется, что бот ошибся или умер, позовите своего преподавателя в отдельном комментарии и опишите проблему:
  для `18.Б09-пу` — `@yeputons`, для `18.Б10-пу` — `@edgarzhavoronkov`
* Если вам захотелось исправить решение после прохождения review, вам требуется:
  * Добавить один или более коммитов с исправлениями
  * Оставить в pull request комментарий `@yeputons-bot, исправлено` (регистр неважен, другие слова не допускаются, допускается добавление точки в конце).
    Например: `@yeputons-bot, ИСПРАВЛЕНО.` или `@yeputons-bot, исправлено`
  * До появления комментария считается, что вы в процессе исправления, и pull request не проверяется
  * Строго соблюдайте формат комментария.
* После вердикта `Approved` или прохождения жёсткого дедлайна pull request закрывается.
  Если вы хотите досдать задачи, требуется открыть новый

# Сроки сдачи
|   | `18.Б09-пу` | `18.Б10-пу` |Максимальное количество баллов при сдаче в срок
|---|---|---|---|
|Дата выдачи|Пятница, 29.03.2019|Пятница, 29.03.2019|   |
|Мягкий дедлайн|Пятница, 05.04.2019, 22:59|Пятница, 05.04.2019, 22:59|5|
|Жёсткий дедлайн|Понедельник, 15.04.2019, 22:59|Понедельник, 15.04.2019, 22:59|2.5|
