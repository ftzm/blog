---
title: Better Access to Relationship Objects in SQLAlchemy
published: 2015-10-25
teaser: Relationship objects make it easy to include additional information about the nature of the relationship between two entities. However, it can still be clumsy to work with them using the standard configuration.
tags: [flask, sqlalchemy, database]
---

I'm in the process of creating a Flask+SQLAlchemy based web app to schedule and record strength training workouts, further details of which I'll likely divulge in a later entry. In this entry I'll just talk about a small tweak I made to my database design to make dealing with association objects simpler.

The design of my application is such that exercises can be associated with multiple workout routines, and can have varying orders within those routines. This requires that the relationships themselves specify the order of exercises. The standard way to deal with extra columns in relationship tables is to use relationship objects.

```Python
class RoutineDayExercise(db.Model):
    __tablename__ = 'routineday_exercises'
    routineday_id = db.Column(db.Integer, db.ForeignKey('routinedays.id'), primary_key=True)
    exercise_id = db.Column(db.Integer, db.ForeignKey('exercises.id'), primary_key=True)
    order = db.Column(db.Integer)
```

However, the standard way to configure these objects, with the combined foreign keys of the related tables constituting the primary keys, was awkward for my setup. The order of an exercise can be manually set by the user at any time, and deleting or re-arranging exercises mean that the order of all exercises in a routine must be frequently adjusted. It's therefore more convenient to access the relationship object directly rather than by going through an Exercise or Routine every time. To this end I eschewed the standard setup and created a table of RoutineExercises wherein each row has its own id and primary key, and the relationship comprises a pair of simple foreign keys.

```Python
class RoutineDayExercise(db.Model):
    __tablename__ = 'routineday_exercises'
    id = db.Column(db.Integer, primary_key=True)
    routineday_id = db.Column(db.Integer, db.ForeignKey('routinedays.id'))
    exercise_id = db.Column(db.Integer, db.ForeignKey('exercises.id'))
    order = db.Column(db.Integer)
```

This setup made the the kinds of operations I had to implement much simpler.The only downside of such a setup is that the database does not automatically protect against duplicate entries as it would with the foreign key - primary key pair. This, however, is easily remedied by manually preventing duplicates in the method for adding new relationships, and/or by simply not presenting users with the ability to create duplicate relationships in the first place. In my case, when users add new exercises to a routine they're only presented with exercises that have yet to be added, which itself prevents duplicates.
