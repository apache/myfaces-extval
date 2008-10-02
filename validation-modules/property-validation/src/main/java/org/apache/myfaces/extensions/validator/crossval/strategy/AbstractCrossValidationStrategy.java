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

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidationStrategy;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.util.CrossValidationUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

/**
 * @author Gerhard Petracek
 */
public abstract class AbstractCrossValidationStrategy extends
        AbstractValidationStrategy implements CrossValidationStrategy
{

    //init cross-validation
    public void processValidation(FacesContext facesContext,
            UIComponent uiComponent, AnnotationEntry annotationEntry,
            Object convertedObject) throws ValidatorException
    {
        CrossValidationStorageEntry entry = getCrossValidationStorageEntry(
                facesContext, uiComponent, annotationEntry, convertedObject);

        CrossValidationUtils.getOrInitCrossValidationStorage().add(entry);
    }

    public CrossValidationStorageEntry getCrossValidationStorageEntry(
            FacesContext facesContext, UIComponent uiComponent,
            AnnotationEntry annotationEntry, Object convertedObject)
    {
        CrossValidationStorageEntry entry = new CrossValidationStorageEntry();

        entry.setAnnotationEntry(annotationEntry);
        entry.setBean(ExtValUtils.getELHelper().getBaseObject(annotationEntry.getValueBindingExpression()));
        entry.setComponent(uiComponent);
        entry.setConvertedObject(convertedObject);
        entry.setValidationStrategy(this);

        return entry;
    }
}
