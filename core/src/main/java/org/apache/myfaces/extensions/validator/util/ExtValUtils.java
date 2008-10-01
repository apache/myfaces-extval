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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractorFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;
import java.lang.annotation.Annotation;


/**
 * @author Gerhard Petracek
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValUtils
{
    public static ValidationStrategy getValidationStrategyForAnnotation(Annotation annotation)
    {
        return ((ClassMappingFactory<Annotation, ValidationStrategy>) ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, ClassMappingFactory.class))
                .create(annotation);
    }

    public static MetaDataExtractor getMetaDataExtractorForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MetaDataExtractor>) ExtValContext
                    .getContext().getFactoryFinder()
                    .getFactory(FactoryNames.META_DATA_EXTRACTOR_FACTORY, ClassMappingFactory.class))
                    .create(validationStrategy);
    }

    public static AnnotationExtractor getAnnotationExtractor()
    {
            return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.COMPONENT_ANNOTATION_EXTRACTOR_FACTORY, AnnotationExtractorFactory.class)
                .create();
    }

    public static void configureComponentWithMetaData(FacesContext facesContext,
                                                      UIComponent uiComponent,
                                                      Map<String, Object> metaData)
    {
        ((ClassMappingFactory<UIComponent, ComponentInitializer>)ExtValContext.getContext().getFactoryFinder()
                    .getFactory(FactoryNames.COMPONENT_INITIALIZER_FACTORY, ClassMappingFactory.class))
                    .create(uiComponent)
                    .configureComponent(facesContext, uiComponent, metaData);
    }

    public static MessageResolver getMessageResolverForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MessageResolver>)ExtValContext.getContext()
            .getFactoryFinder()
            .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, ClassMappingFactory.class))
            .create(validationStrategy);
    }
}
