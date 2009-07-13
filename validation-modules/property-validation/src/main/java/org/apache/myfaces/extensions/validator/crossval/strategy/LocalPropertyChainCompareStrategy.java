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

import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.crossval.storage.ProcessedInformationStorage;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.CrossValidationUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;

import java.lang.annotation.Annotation;

/**
 * "[local_property.property1.property2]"
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class LocalPropertyChainCompareStrategy extends LocalCompareStrategy
{
    @Override
    public boolean evaluateReferenceAndValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        if(!validationTarget.contains("."))
        {
            //not supported - TODO add logging
            return false;
        }

        return tryToValidateLocally(
            crossValidationStorageEntry,
            crossValidationStorage,
            validationTarget,
            compareStrategy);
    }

    @Override
    protected boolean tryToValidateLocally(CrossValidationStorageEntry crossValidationStorageEntry,
                                           CrossValidationStorage crossValidationStorage,
                                           String targetKey,
                                           AbstractCompareStrategy compareStrategy)
    {
        ProcessedInformationStorage processedInformationStorage =
                CrossValidationUtils.getOrInitProcessedInformationStorage();

        String newKey = createTargetKey(crossValidationStorageEntry, targetKey);

        if (processedInformationStorage.containsEntry(newKey))
        {
            ProcessedInformationEntry validationTargetEntry = processedInformationStorage.getEntry(newKey);

            processCrossComponentValidation(compareStrategy, crossValidationStorageEntry, validationTargetEntry);
        }
        //no target - because there is no target component - value was validated against the model
        else
        {
            processModelAwareCrossValidation(compareStrategy, crossValidationStorageEntry, targetKey);
        }

        return true;
    }

    private void processCrossComponentValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry,
            ProcessedInformationEntry targetInformationEntry)
    {
        if (compareStrategy.isViolation(
                crossValidationStorageEntry.getConvertedObject(),
                targetInformationEntry.getConvertedValue(),
                crossValidationStorageEntry.getMetaDataEntry().getValue(Annotation.class)))
        {
            CrossValidationStorageEntry tmpCrossValidationStorageEntry = new CrossValidationStorageEntry();
            tmpCrossValidationStorageEntry.setComponent(crossValidationStorageEntry.getComponent());
            tmpCrossValidationStorageEntry.setClientId(targetInformationEntry.getClientId());
            tmpCrossValidationStorageEntry.setConvertedObject(targetInformationEntry.getConvertedValue());
            tmpCrossValidationStorageEntry.setValidationStrategy(compareStrategy);

            //process after violation
            //just add messages
            if(crossValidationStorageEntry.getComponent() != null)
            {
                compareStrategy.processTargetComponentAfterViolation(
                        crossValidationStorageEntry, tmpCrossValidationStorageEntry);
            }
            else
            {
                compareStrategy.processTargetComponentAfterViolation(crossValidationStorageEntry, null);
            }

            //thow exception
            compareStrategy.processSourceComponentAfterViolation(crossValidationStorageEntry);
        }
    }

    private void processModelAwareCrossValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry, String targetKey)
    {
        PropertyDetails propertyDetails = crossValidationStorageEntry.getMetaDataEntry()
            .getProperty(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        Object newBase = ReflectionUtils
            .getBaseOfPropertyChain(propertyDetails.getBaseObject(), targetKey);

        if(targetKey.contains("."))
        {
            //find the last property
            targetKey = targetKey.substring(targetKey.lastIndexOf(".") + 1, targetKey.length());
        }

        Object targetValue = getValueOfProperty(newBase, targetKey);

        ProcessedInformationEntry targetEntry = new ProcessedInformationEntry();
        targetEntry.setBean(newBase);
        targetEntry.setConvertedValue(targetValue);

        CrossValidationHelper
                .crossValidateCompareStrategy(
                        compareStrategy, crossValidationStorageEntry, targetEntry, true);
    }
}