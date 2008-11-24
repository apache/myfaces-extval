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

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractAnnotationValidationStrategy;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.util.CrossValidationUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public abstract class AbstractCrossValidationStrategy extends
    AbstractAnnotationValidationStrategy implements CrossValidationStrategy
{
    //init cross-validation
    public void processValidation(FacesContext facesContext,
            UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject) throws ValidatorException
    {
        CrossValidationStorageEntry entry =
            getCrossValidationStorageEntry(facesContext, uiComponent, metaDataEntry, convertedObject);

        CrossValidationUtils.getOrInitCrossValidationStorage().add(entry);
    }

    public CrossValidationStorageEntry getCrossValidationStorageEntry(
            FacesContext facesContext, UIComponent uiComponent,
            MetaDataEntry metaDataEntry, Object convertedObject)
    {
        CrossValidationStorageEntry entry = new CrossValidationStorageEntry();

        entry.setMetaDataEntry(metaDataEntry);
        entry.setComponent(uiComponent);
        entry.setClientId(uiComponent.getClientId(facesContext));
        entry.setConvertedObject(convertedObject);
        entry.setValidationStrategy(this);

        return entry;
    }

    @Override
    protected final boolean processAfterValidatorException(FacesContext facesContext,
                                                           UIComponent uiComponent,
                                                           MetaDataEntry metaDataEntry,
                                                           Object convertedObject,
                                                           ValidatorException validatorException)
    {
        throw new IllegalStateException("not available for cross validation");
    }

    @Override
    protected final String getLabel(FacesContext facesContext, UIComponent uiComponent, MetaDataEntry metaDataEntry)
    {
        throw new IllegalStateException("not available for cross validation - use processAfterCrossValidatorException");
    }

    @Override
    protected final void initValidation(FacesContext facesContext,
                                        UIComponent uiComponent,
                                        MetaDataEntry metaDataEntry,
                                        Object convertedObject)
    {
        //not available for cross validation - use initCrossValidation
    }

    protected void initCrossValidation(CrossValidationStorageEntry crossValidationStorageEntry)
    {
        //override if needed
    }

    //override if needed
    protected boolean processAfterCrossValidatorException(CrossValidationStorageEntry crossValidationStorageEntry,
                                                          ValidatorException validatorException)
    {
        return ExtValUtils.executeAfterThrowingInterceptors(
                crossValidationStorageEntry.getComponent(), crossValidationStorageEntry.getMetaDataEntry(),
                crossValidationStorageEntry.getConvertedObject(), validatorException);
    }
}
