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
package org.apache.myfaces.extensions.validator.crossval.referencing.strategy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.strategy.AbstractCompareStrategy;
import org.apache.myfaces.extensions.validator.util.ELUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * "[property_name]" ... local validation -> cross-component, but no cross-entity validation
 *
 * @author Gerhard Petracek
 */
public class LocalCompareStrategy implements ReferencingStrategy
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean evalReferenceAndValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        tryToValidateLocally(crossValidationStorageEntry, validationTarget,
                compareStrategy);

        return true;
    }

    protected void tryToValidateLocally(
            CrossValidationStorageEntry crossValidationStorageEntry,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        boolean violationFound = false;

        String baseValueBindingExpression = crossValidationStorageEntry
                .getAnnotationEntry().getValueBindingExpression();
        baseValueBindingExpression = baseValueBindingExpression.substring(0,
                baseValueBindingExpression.lastIndexOf("."));

        String targetValueBindingExpression;

        FacesContext facesContext = FacesContext.getCurrentInstance();

        Map<String, ProcessedInformationEntry> valueBindingConvertedValueMapping = ExtValUtils
                .getOrInitValueBindingConvertedValueMapping();
        ProcessedInformationEntry validationTargetEntry;

        targetValueBindingExpression = baseValueBindingExpression + "."
                + validationTarget + "}";

        if (!ELUtils.isExpressionValid(facesContext,
                targetValueBindingExpression))
        {
            return;
        }

        if (!valueBindingConvertedValueMapping
                .containsKey(targetValueBindingExpression))
        {
            return;
        }
        validationTargetEntry = compareStrategy.resolveValidationTargetEntry(
                valueBindingConvertedValueMapping,
                targetValueBindingExpression, crossValidationStorageEntry
                        .getBean());

        if (validationTargetEntry == null)
        {
            logger.warn("couldn't find converted object for "
                    + targetValueBindingExpression);
            return;
        }

        if (compareStrategy.isViolation(crossValidationStorageEntry
                .getConvertedObject(), validationTargetEntry
                .getConvertedValue(), crossValidationStorageEntry
                .getAnnotationEntry().getAnnotation()))
        {

            CrossValidationStorageEntry tmpCrossValidationStorageEntry = new CrossValidationStorageEntry();
            if (compareStrategy
                    .useTargetComponentToDisplayErrorMsg(crossValidationStorageEntry))
            {
                tmpCrossValidationStorageEntry
                        .setComponent(validationTargetEntry.getComponent());
            }
            else
            {
                tmpCrossValidationStorageEntry
                        .setComponent(crossValidationStorageEntry
                                .getComponent());
            }
            tmpCrossValidationStorageEntry
                    .setConvertedObject(validationTargetEntry
                            .getConvertedValue());
            tmpCrossValidationStorageEntry
                    .setValidationStrategy(compareStrategy);

            compareStrategy
                    .processTargetComponentAfterViolation(
                            crossValidationStorageEntry,
                            tmpCrossValidationStorageEntry);

            violationFound = true;
        }

        if (violationFound)
        {
            compareStrategy
                    .processSourceComponentAfterViolation(crossValidationStorageEntry);
        }
    }
}