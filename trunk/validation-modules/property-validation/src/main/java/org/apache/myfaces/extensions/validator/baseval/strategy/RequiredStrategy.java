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
package org.apache.myfaces.extensions.validator.baseval.strategy;

import org.apache.myfaces.extensions.validator.baseval.annotation.Required;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidationStrategy;
import org.apache.myfaces.extensions.validator.core.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.MetaDataKeys;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import java.lang.annotation.Annotation;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 */
public class RequiredStrategy extends AbstractValidationStrategy implements MetaDataExtractor
{
    public void processValidation(FacesContext facesContext,
            UIComponent uiComponent, AnnotationEntry annotationEntry,
            Object convertedObject) throws ValidatorException
    {
        if (convertedObject == null || convertedObject.equals(""))
        {
            throw new ValidatorException(
                    getValidationErrorFacesMassage(annotationEntry
                            .getAnnotation()));
        }
    }

    public Map<String, Object> extractMetaData(Annotation annotation)
    {
        Map<String, Object> results = new HashMap<String, Object>();
        results.put(MetaDataKeys.REQUIRED, true);
        return results;
    }

    protected String getValidationErrorMsgKey(Annotation annotation)
    {
        return ((Required) annotation).validationErrorMsgKey();
    }
}