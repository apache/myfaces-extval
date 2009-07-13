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

import org.apache.myfaces.extensions.validator.crossval.storage.ProcessedInformationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.storage.ProcessedInformationStorage;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.util.CrossValidationUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.lang.reflect.Method;

/**
 * "[property_name]" ... local validation -> cross-component, but no cross-entity validation
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class LocalCompareStrategy implements ReferencingStrategy
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean evaluateReferenceAndValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        if(validationTarget.contains("."))
        {
            //LocalPropertyChainCompareStrategy will continue
            return false;
        }

        return tryToValidateLocally(
            crossValidationStorageEntry,
            crossValidationStorage,
            validationTarget,
            compareStrategy);
    }

    protected boolean tryToValidateLocally(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String targetKey,
            AbstractCompareStrategy compareStrategy)
    {
        ProcessedInformationStorage processedInformationStorage =
                CrossValidationUtils.getOrInitProcessedInformationStorage();

        boolean isModelAwareValidation =
                isModelAwareCrossValidation(crossValidationStorageEntry, processedInformationStorage, targetKey);

        String targetProperty = targetKey;

        String sourceKey = resolveSourceKey(crossValidationStorageEntry);
        targetKey = sourceKey.substring(0, sourceKey.lastIndexOf(".") + 1) + targetKey;

        ProcessedInformationStorageEntry validationTargetEntry = CrossValidationUtils.resolveValidationTargetEntry(
                processedInformationStorage, targetKey, crossValidationStorageEntry);

        if (validationTargetEntry != null && validationTargetEntry.getComponent() != null && !isModelAwareValidation)
        {
            processCrossComponentValidation(compareStrategy, crossValidationStorageEntry, validationTargetEntry);
        }
        //no target - because there is no target component - value was validated against the model
        else if(isModelAwareValidation)
        {
            processModelAwareCrossValidation(compareStrategy, crossValidationStorageEntry, targetProperty);
        }
        else
        {
            unsupportedCase(crossValidationStorageEntry);
        }

        return true;
    }

    protected String createTargetKey(CrossValidationStorageEntry crossValidationStorageEntry, String targetKey)
    {
        //no real value binding expression
        //ValueBindingExpression just hepls to replace the property of the key
        //here only dot-notation is allowed -> no problem
        ValueBindingExpression baseExpression =
            new ValueBindingExpression("#{" + crossValidationStorageEntry.getMetaDataEntry()
                .getProperty(PropertyInformationKeys.PROPERTY_DETAILS,
                    PropertyDetails.class).getKey() + "}");

        String result = ValueBindingExpression.replaceOrAddProperty(baseExpression, targetKey)
            .getExpressionString();
        return result.substring(2, result.length() -1);
    }

    protected Object getValueOfProperty(Object base, String property)
    {
        property = property.substring(0,1).toUpperCase() + property.substring(1, property.length());
        Method targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "get" + property);

        if(targetMethod == null)
        {
            targetMethod = ReflectionUtils.tryToGetMethod(base.getClass(), "is" + property);
        }

        if(targetMethod == null)
        {
            throw new IllegalStateException(
                "class " + base.getClass() + " has no public get/is " + property.toLowerCase());
        }
        return ReflectionUtils.tryToInvokeMethod(base, targetMethod);
    }

    private boolean isModelAwareCrossValidation(
            CrossValidationStorageEntry crossValidationStorageEntry,
            ProcessedInformationStorage keyConvertedValueMapping,
            String targetKey)
    {
        String newKey = createTargetKey(crossValidationStorageEntry, targetKey);

        return !keyConvertedValueMapping.containsEntry(newKey);

    }

    private String resolveSourceKey(CrossValidationStorageEntry crossValidationStorageEntry)
    {
        PropertyDetails propertyDetails = crossValidationStorageEntry.getMetaDataEntry()
                .getProperty(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        String sourceKey = propertyDetails.getKey();

        if(!sourceKey.contains("."))
        {
            throw new IllegalStateException("source path: " + sourceKey + " invalid");
        }

        return sourceKey;
    }

    private void unsupportedCase(CrossValidationStorageEntry crossValidationStorageEntry)
    {
        if(logger.isWarnEnabled())
        {
            logger.warn("couldn't find converted object for " +  crossValidationStorageEntry.getMetaDataEntry()
            .getProperty(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class).getKey());
        }
    }

    private void processCrossComponentValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry,
            ProcessedInformationStorageEntry validationTargetEntry)
    {
        CrossValidationHelper
                .crossValidateCompareStrategy(
                        compareStrategy, crossValidationStorageEntry, validationTargetEntry, false);
    }

    private void processModelAwareCrossValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry,
            String targetProperty)
    {
        ProcessedInformationStorageEntry targetEntry = new ProcessedInformationStorageEntry();

        targetEntry.setBean(
                crossValidationStorageEntry.getMetaDataEntry()
                        .getProperty(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class).getBaseObject());
        targetEntry
                .setConvertedValue(getValueOfProperty(targetEntry.getBean(), targetProperty));

        CrossValidationHelper
                .crossValidateCompareStrategy(compareStrategy, crossValidationStorageEntry, targetEntry, true);
    }
}
