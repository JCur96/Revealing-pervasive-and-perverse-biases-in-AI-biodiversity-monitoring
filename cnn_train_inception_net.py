#!/usr/bin/env python3

import timeit
import os
import numpy as np
import pandas as pd
import argparse
from sklearn.metrics import confusion_matrix
from tensorflow.keras.applications import InceptionV3
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.models import Model, model_from_json
from tensorflow.keras.layers import Dense, Dropout, GlobalAveragePooling2D
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import LearningRateScheduler, ModelCheckpoint, Callback
from keras.regularizers import l2

# Argument Parsing
parser = argparse.ArgumentParser()
parser.add_argument("-tnd", "--train_dir", type=str, required=True)
parser.add_argument("-ttd", "--test_dir", type=str, required=True)
parser.add_argument("-mspf", "--model_save_path_final", type=str, required=True)
parser.add_argument("-mspi", "--model_save_path_interim", type=str, required=True)
parser.add_argument("-hsp", "--history_save_path", type=str, required=True)
parser.add_argument("-casp", "--class_accuracy_save_path", type=str, required=True)
args = parser.parse_args()

print(args.train_dir)

# Per-Class Accuracy Callback
class PerClassAccuracy(Callback):
    def on_epoch_end(self, epoch, logs=None):
        predictions = np.argmax(self.model.predict(test_set), axis=1)
        true_classes = test_set.classes
        conf_matrix = confusion_matrix(true_classes, predictions, labels=list(range(num_classes)))
        per_class_accuracy = conf_matrix.diagonal() / conf_matrix.sum(axis=1)
        logs = logs or {}
        for idx, acc in enumerate(per_class_accuracy):
            logs[f'accuracy_class_{idx}'] = acc
        print(f"Epoch {epoch + 1}: Per-Class Accuracy: {per_class_accuracy}")

# Determine number of classes
num_classes = len(next(os.walk(args.train_dir))[1])

# Start Timer
tic = timeit.default_timer()

# Data Augmentation
train_datagen = ImageDataGenerator(rescale=1./255, shear_range=0.2, horizontal_flip=True, brightness_range=[0.8, 1.2])
test_datagen = ImageDataGenerator(rescale=1./255)

# Load Training and Test Data
training_set = train_datagen.flow_from_directory(args.train_dir, target_size=(224, 224), batch_size=128, class_mode='categorical')
test_set = test_datagen.flow_from_directory(args.test_dir, target_size=(224, 224), batch_size=128, class_mode='categorical')

# Steps per epoch
steps_per_epoch = max(training_set.samples // training_set.batch_size, 1)
validation_steps = max(test_set.samples // test_set.batch_size, 1)

# Load Base Model
base_model = InceptionV3(weights="imagenet", include_top=False, input_shape=(224,224,3))
# base_model.load_weights('../../model_weights/inception_v3_weights_tf_dim_ordering_tf_kernels_notop.h5')

# Add Custom Layers
x = base_model.output
x = GlobalAveragePooling2D()(x)
x = Dropout(0.5)(x)
custom_out = Dense(num_classes, activation='softmax', name='softmax')(x)
model = Model(inputs=base_model.input, outputs=custom_out)
print(model.summary())
model.save_weights("tmpi.weights.h5")

# Learning Rate Scheduler
def scheduler(epoch):
    return {0: 0.0001, 5: 0.0001, 10: 0.0001, 20: 0.00005, 30: 0.00001, 35: 0.000005}.get(epoch, 0.000001)

dynamic_lr = LearningRateScheduler(scheduler, verbose=2)

# Model Checkpoint
checkpoint = ModelCheckpoint(args.model_save_path_interim, monitor='val_accuracy', verbose=2, save_best_only=True, mode='max')
callbacks_list = [checkpoint, dynamic_lr]

# Apply L2 Regularization
regularizer = l2(0.0005 / 2)
for layer in model.layers:
    if hasattr(layer, 'kernel_regularizer') and layer.trainable:
        layer.kernel_regularizer = regularizer

# Reload Weights
out = model_from_json(model.to_json())
out.load_weights("tmpi.weights.h5")
model = out

# Compile Model
model.compile(optimizer=Adam(), loss='categorical_crossentropy', metrics=['accuracy'])

# Add Per-Class Accuracy Callback
callbacks_list.append(PerClassAccuracy())

# Train Model
history = model.fit(
    training_set,
    steps_per_epoch=steps_per_epoch,
    epochs=40,
    validation_data=test_set,
    validation_steps=validation_steps,
    callbacks=callbacks_list
)

# Save Per-Class Accuracies
class_accuracies = {key: val for key, val in history.history.items() if 'accuracy_class_' in key}
class_accuracy_df = pd.DataFrame(class_accuracies)
class_accuracy_df.to_csv(args.class_accuracy_save_path)
print(class_accuracy_df)

# Save Training History
hist_df = pd.DataFrame(history.history)
hist_df.to_csv(args.history_save_path)

# Save Final Model
model.save(args.model_save_path_final)

# End Timer
toc = timeit.default_timer()
print(f"Training completed in {toc - tic:.2f} seconds")
