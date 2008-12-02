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

import org.apache.myfaces.extensions.validator.crossval.ProcessedInformationEntry;
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
            ProcessedInformationEntry validationTargetEntry)
    {
        boolean violationFound = false;

        if (compareStrategy.isViolation(
                crossValidationStorageEntry.getConvertedObject(),
                validationTargetEntry.getConvertedValue(),
                crossValidationStorageEntry.getMetaDataEntry().getValue(Annotation.class)))
        {

            CrossValidationStorageEntry tmpCrossValidationStorageEntry = new CrossValidationStorageEntry();
            if (compareStrategy.useTargetComponentToDisplayErrorMsg(crossValidationStorageEntry))
            {
                tmpCrossValidationStorageEntry.setComponent(validationTargetEntry.getComponent());
                tmpCrossValidationStorageEntry.setClientId(validationTargetEntry.getClientId());
            }
            else
            {
                tmpCrossValidationStorageEntry.setComponent(crossValidationStorageEntry.getComponent());
                tmpCrossValidationStorageEntry.setClientId(crossValidationStorageEntry.getClientId());
            }
            tmpCrossValidationStorageEntry.setConvertedObject(validationTargetEntry.getConvertedValue());
            tmpCrossValidationStorageEntry.setValidationStrategy(compareStrategy);

            compareStrategy
                    .processTargetComponentAfterViolation(crossValidationStorageEntry, tmpCrossValidationStorageEntry);

            violationFound = true;
        }

        if (violationFound)
        {
            compareStrategy.processSourceComponentAfterViolation(crossValidationStorageEntry);
        }
    }
}