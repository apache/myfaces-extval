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
package org.apache.myfaces.extensions.validator.baseval.metadata.extractor;

import org.apache.myfaces.extensions.validator.baseval.annotation.JoinValidation;
import org.apache.myfaces.extensions.validator.baseval.annotation.extractor.DefaultPropertyScanningAnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;

import javax.faces.context.FacesContext;
import java.lang.annotation.Annotation;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
public class JoinMetaDataExtractor implements MetaDataExtractor
{
    public Map<String, Object> extractMetaData(Annotation annotation)
    {
        AnnotationExtractor extractor = new DefaultPropertyScanningAnnotationExtractor();

        String[] targetExpressions = ((JoinValidation)annotation).value();

        ValidationStrategy validationStrategy;
        MetaDataExtractor metaDataExtractor;

        Map<String, Object> results = new HashMap<String, Object>();

        for (String targetExpression : targetExpressions)
        {
            for (AnnotationEntry entry : extractor
                                            .extractAnnotations(FacesContext.getCurrentInstance(), targetExpression))
            {
                validationStrategy = ((ClassMappingFactory<Annotation, ValidationStrategy>) ExtValContext.getContext()
                    .getFactoryFinder()
                    .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, ClassMappingFactory.class))
                    .create(entry.getAnnotation());

                metaDataExtractor = ((ClassMappingFactory<ValidationStrategy, MetaDataExtractor>) ExtValContext
                    .getContext().getFactoryFinder()
                    .getFactory(FactoryNames.META_DATA_EXTRACTOR_FACTORY, ClassMappingFactory.class))
                    .create(validationStrategy);

                if (metaDataExtractor != null)
                {
                    results.putAll(metaDataExtractor.extractMetaData(entry.getAnnotation()));
                }
            }
        }
        return results;
    }
}
