/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.crossval.strategy;

import org.apache.myfaces.extensions.validator.crossval.storage.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class CrossValidationHelper
{
    public static void crossValidateCompareStrategy(AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry,
            ProcessedInformationEntry validationTargetEntry,
            boolean isModelAwareValidation)
    {
        if (compareStrategy.isViolation(
                crossValidationStorageEntry.getConvertedObject(),
                validationTargetEntry.getConvertedValue(),
                crossValidationStorageEntry.getMetaDataEntry().getValue(Annotation.class)))
        {
            //process after violation
            //just add messages
            if(!isModelAwareValidation)
            {
                processTargetAfterCrossComponentValidation(
                        compareStrategy, crossValidationStorageEntry, validationTargetEntry);
            }
            else
            {
                processTargetAfterModelAwareCrossValidation(
                        compareStrategy, crossValidationStorageEntry);
            }

            //thow exception
            compareStrategy.processSourceComponentAfterViolation(crossValidationStorageEntry);
        }
    }

    private static void processTargetAfterCrossComponentValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry sourceCrossValidationStorageEntry,
            ProcessedInformationEntry validationTargetEntry)
    {
        CrossValidationStorageEntry targetCrossValidationStorageEntry = new CrossValidationStorageEntry();

        if (compareStrategy.useTargetComponentToDisplayErrorMsg(sourceCrossValidationStorageEntry))
        {
            targetCrossValidationStorageEntry.setComponent(validationTargetEntry.getComponent());
            targetCrossValidationStorageEntry.setClientId(validationTargetEntry.getClientId());
        }
        else
        {
            targetCrossValidationStorageEntry.setComponent(sourceCrossValidationStorageEntry.getComponent());
            targetCrossValidationStorageEntry.setClientId(sourceCrossValidationStorageEntry.getClientId());
        }

        targetCrossValidationStorageEntry.setConvertedObject(validationTargetEntry.getConvertedValue());
        targetCrossValidationStorageEntry.setValidationStrategy(compareStrategy);

        //add message
        compareStrategy.processTargetComponentAfterViolation(
                sourceCrossValidationStorageEntry, targetCrossValidationStorageEntry);
    }

    private static void processTargetAfterModelAwareCrossValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry)
    {
        //no target - because there is no target component - value was validated against the model
        compareStrategy.processTargetComponentAfterViolation(crossValidationStorageEntry, null);
    }
}
