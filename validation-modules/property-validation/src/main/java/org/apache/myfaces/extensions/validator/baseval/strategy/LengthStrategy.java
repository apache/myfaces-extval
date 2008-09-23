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

import org.apache.myfaces.extensions.validator.baseval.annotation.Length;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.AbstractValidatorAdapter;
import org.apache.myfaces.extensions.validator.core.MetaDataExtractor;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.LengthValidator;
import javax.faces.validator.ValidatorException;
import java.lang.annotation.Annotation;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 */
public class LengthStrategy extends AbstractValidatorAdapter implements MetaDataExtractor
{
    protected void processValidation(FacesContext facesContext,
            UIComponent uiComponent, AnnotationEntry annotationEntry,
            Object convertedObject) throws ValidatorException
    {

        Length annotation = annotationEntry.getAnnotation(Length.class);
        LengthValidator lengthValidator = new LengthValidator();

        lengthValidator.setMinimum(annotation.minimum());
        lengthValidator.setMaximum(annotation.maximum());

        lengthValidator.validate(facesContext, uiComponent, convertedObject);
    }

    public Map<String, Object> extractMetaData(Annotation annotation)
    {
        Map<String, Object> results = new HashMap<String, Object>();
        int minimum = ((Length)annotation).minimum();
        results.put("min_length", minimum);
        results.put("max_length", ((Length)annotation).maximum());

        if(minimum > 0)
        {
            results.put("required", true);
        }

        return results;
    }
}